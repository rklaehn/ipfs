package org.ipfs.api;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.stream.*;

public class IPFS {
    public enum PinType {all, direct, indirect, recursive}
    public List<String> ObjectTemplates = Arrays.asList("unixfs-dir");
    public List<String> ObjectPatchTypes = Arrays.asList("add-link", "rm-link", "set-data", "append-data");

    public final String host;
    public final int port;
    private final String version;
    public final Pin pin = new Pin();
    public final Repo repo = new Repo();
    public final IPFSObject object = new IPFSObject();
    public final Swarm swarm = new Swarm();
    public final Bootstrap bootstrap = new Bootstrap();
    public final Block block = new Block();
    public final Diag diag = new Diag();
    public final Config config = new Config();
    public final Refs refs = new Refs();
    public final Update update = new Update();
    public final DHT dht = new DHT();
    public final File file = new File();
    public final Stats stats = new Stats();
    public final Name name = new Name();

    public IPFS(String host, int port) {
        this(host, port, "/api/v0/");
    }

    public IPFS(String multiaddr) {
        this(new MultiAddress(multiaddr));
    }

    public IPFS(MultiAddress addr) {
        this(addr.getHost(), addr.getTCPPort(), "/api/v0/");
    }

    public IPFS(String host, int port, String version) {
        this.host = host;
        this.port = port;
        this.version = version;
    }

    public MerkleNode add(NamedStreamable file) throws IOException {
        return add(Arrays.<NamedStreamable>asList(file)).get(0);
    }

    public List<MerkleNode> add(List<NamedStreamable> files) throws IOException {
        Multipart m = new Multipart("http://" + host + ":" + port + version+"add?stream-channels=true", "UTF-8");
        for (NamedStreamable f : files)
            m.addFilePart("file", f);
        String res = m.finish();
        return JSONParser.parseStream(res).stream().map(x -> MerkleNode.fromJSON((Map<String, Object>) x)).collect(Collectors.toList());
    }

    public List<MerkleNode> ls(Multihash hash) throws IOException {
        Map res = retrieveMap("ls/" + hash);
        return ((List<Object>) res.get("Objects")).stream().map(x -> MerkleNode.fromJSON((Map) x)).collect(Collectors.toList());
    }

    public byte[] cat(Multihash hash) throws IOException {
        return retrieve("cat/" + hash);
    }

    public byte[] get(Multihash hash) throws IOException {
        return retrieve("get/" + hash);
    }

    public InputStream catStream(Multihash hash) throws IOException {
        return retrieveStream("cat/" + hash);
    }

    public Map refs(Multihash hash, boolean recursive) throws IOException {
        return retrieveMap("refs?arg=" + hash +"&r="+recursive);
    }

    public Map resolve(String scheme, Multihash hash, boolean recursive) throws IOException {
        return retrieveMap("resolve?arg=/" + scheme+"/"+hash +"&r="+recursive);
    }


    public String dns(String domain) throws IOException {
        Map res = retrieveMap("dns?arg=" + domain);
        return (String)res.get("Path");
    }

    public Map mount(java.io.File ipfsRoot, java.io.File ipnsRoot) throws IOException {
        if (ipfsRoot != null && !ipfsRoot.exists())
            ipfsRoot.mkdirs();
        if (ipnsRoot != null && !ipnsRoot.exists())
            ipnsRoot.mkdirs();
        return (Map)retrieveAndParse("mount?arg=" + (ipfsRoot != null ? ipfsRoot.getPath() : "/ipfs" ) + "&arg=" +
                (ipnsRoot != null ? ipnsRoot.getPath() : "/ipns" ));
    }

    // level 2 commands
    public class Refs {
        public List<Multihash> local() throws IOException {
            return Arrays.asList(new String(retrieve("refs/local")).split("\n")).stream().map(Multihash::fromBase58).collect(Collectors.toList());
        }
    }

    /* Pinning an object ensures a local copy of it is kept.
     */
    public class Pin {
        public List<Multihash> add(Multihash hash) throws IOException {
            return ((List<Object>)((Map)retrieveAndParse("pin/add?stream-channels=true&arg=" + hash)).get("Pins"))
                    .stream()
                    .map(x -> Multihash.fromBase58((String)x))
                    .collect(Collectors.toList());
        }

        public Map<Multihash, Object> ls() throws IOException {
            return ls(PinType.direct);
        }

        public Map<Multihash, Object> ls(PinType type) throws IOException {
            return ((Map<String, Object>)(((Map)retrieveAndParse("pin/ls?stream-channels=true&t="+type.name())).get("Keys"))).entrySet()
                    .stream()
                    .collect(Collectors.toMap(x -> Multihash.fromBase58(x.getKey()), x-> x.getValue()));
        }

        public List<Multihash> rm(Multihash hash) throws IOException {
            return rm(hash, true);
        }

        public List<Multihash> rm(Multihash hash, boolean recursive) throws IOException {
            Map json = retrieveMap("pin/rm?stream-channels=true&r=" + recursive + "&arg=" + hash);
            return ((List<Object>) json.get("Pins")).stream().map(x -> Multihash.fromBase58((String) x)).collect(Collectors.toList());
        }
    }

    /* 'ipfs repo' is a plumbing command used to manipulate the repo.
     */
    public class Repo {
        public Object gc() throws IOException {
            return retrieveAndParse("repo/gc");
        }
    }

    /* 'ipfs block' is a plumbing command used to manipulate raw ipfs blocks.
     */
    public class Block {
        public byte[] get(Multihash hash) throws IOException {
            return retrieve("block/get?stream-channels=true&arg=" + hash);
        }

        public List<MerkleNode> put(List<byte[]> data) throws IOException {
            Multipart m = new Multipart("http://" + host + ":" + port + version+"block/put?stream-channels=true", "UTF-8");
            for (byte[] f : data)
                m.addFilePart("file", new NamedStreamable.ByteArrayWrapper(f));
            String res = m.finish();
            return JSONParser.parseStream(res).stream().map(x -> MerkleNode.fromJSON((Map<String, Object>) x)).collect(Collectors.toList());
        }

        public Map stat(Multihash hash) throws IOException {
            return retrieveMap("block/stat?stream-channels=true&arg=" + hash);
        }
    }

    /* 'ipfs object' is a plumbing command used to manipulate DAG objects directly. {Object} is a subset of {Block}
     */
    public class IPFSObject {
        public List<MerkleNode> put(List<byte[]> data) throws IOException {
            Multipart m = new Multipart("http://" + host + ":" + port + version+"object/put?stream-channels=true", "UTF-8");
            for (byte[] f : data)
                m.addFilePart("file", new NamedStreamable.ByteArrayWrapper(f));
            String res = m.finish();
            return JSONParser.parseStream(res).stream().map(x -> MerkleNode.fromJSON((Map<String, Object>) x)).collect(Collectors.toList());
        }

        public List<MerkleNode> put(String encoding, List<byte[]> data) throws IOException {
            if (!"json".equals(encoding) && !"protobuf".equals(encoding))
                throw new IllegalArgumentException("Encoding must be json or protobuf");
            Multipart m = new Multipart("http://" + host + ":" + port + version+"object/put?stream-channels=true&encoding="+encoding, "UTF-8");
            for (byte[] f : data)
                m.addFilePart("file", new NamedStreamable.ByteArrayWrapper(f));
            String res = m.finish();
            return JSONParser.parseStream(res).stream().map(x -> MerkleNode.fromJSON((Map<String, Object>) x)).collect(Collectors.toList());
        }

        public MerkleNode get(Multihash hash) throws IOException {
            Map json = retrieveMap("object/get?stream-channels=true&arg=" + hash);
            json.put("Hash", hash.toBase58());
            return MerkleNode.fromJSON(json);
        }

        public MerkleNode links(Multihash hash) throws IOException {
            Map json = retrieveMap("object/links?stream-channels=true&arg=" + hash);
            return MerkleNode.fromJSON(json);
        }

        public Map<String, Object> stat(Multihash hash) throws IOException {
            return retrieveMap("object/stat?stream-channels=true&arg=" + hash);
        }

        public byte[] data(Multihash hash) throws IOException {
            return retrieve("object/data?stream-channels=true&arg=" + hash);
        }

        public MerkleNode _new(Optional<String> template) throws IOException {
            if (template.isPresent() && !ObjectTemplates.contains(template.get()))
                throw new IllegalStateException("Unrecognised template: "+template.get());
            Map json = retrieveMap("object/new?stream-channels=true"+(template.isPresent() ? "&arg=" + template.get() : ""));
            return MerkleNode.fromJSON(json);
        }

        public MerkleNode patch(Multihash base, String command, Optional<byte[]> data, Optional<String> name, Optional<Multihash> target) throws IOException {
            if (!ObjectPatchTypes.contains(command))
                throw new IllegalStateException("Illegal Object.patch command type: "+command);
            String targetPath = "object/patch/"+command+"?arg=" + base.toBase58();
            if (name.isPresent())
                targetPath += "&arg=" + name.get();
            if (target.isPresent())
                targetPath += "&arg=" + target.get().toBase58();

            switch (command) {
                case "add-link":
                    if (!target.isPresent())
                        throw new IllegalStateException("add-link requires name and target!");
                case "rm-link":
                    if (!name.isPresent())
                        throw new IllegalStateException("link name is required!");
                    return MerkleNode.fromJSON(retrieveMap(targetPath));
                case "set-data":
                case "append-data":
                    if (!data.isPresent())
                        throw new IllegalStateException("set-data requires data!");
                    Multipart m = new Multipart("http://" + host + ":" + port + version+"object/patch/"+command+"?arg="+base.toBase58()+"&stream-channels=true", "UTF-8");
                    m.addFilePart("file", new NamedStreamable.ByteArrayWrapper(data.get()));
                    String res = m.finish();
                    return MerkleNode.fromJSON(JSONParser.parse(res));

                default:
                    throw new IllegalStateException("Unimplemented");
            }
        }
    }

    public class Name {
        public Map publish(Multihash hash) throws IOException {
            return publish(Optional.empty(), hash);
        }

        public Map publish(Optional<String> id, Multihash hash) throws IOException {
            return retrieveMap("name/publish?arg=" + (id.isPresent() ? id+"&arg=" : "") + "/ipfs/"+hash);
        }

        public String resolve(Multihash hash) throws IOException {
            Map res = (Map) retrieveAndParse("name/resolve?arg=" + hash);
            return (String)res.get("Path");
        }
    }

    public class DHT {
        public Map findprovs(Multihash hash) throws IOException {
            return retrieveMap("dht/findprovs?arg=" + hash);
        }

        public Map query(MultiAddress addr) throws IOException {
            return retrieveMap("dht/query?arg=" + addr.toString());
        }

        public Map findpeer(MultiAddress addr) throws IOException {
            return retrieveMap("dht/findpeer?arg=" + addr.toString());
        }

        public Map get(Multihash hash) throws IOException {
            return retrieveMap("dht/get?arg=" + hash);
        }

        public Map put(String key, String value) throws IOException {
            return retrieveMap("dht/put?arg=" + key + "&arg="+value);
        }
    }

    public class File {
        public Map ls(Multihash path) throws IOException {
            return retrieveMap("file/ls?arg=" + path);
        }
    }

    // Network commands

    public List<MultiAddress> bootstrap() throws IOException {
        return ((List<String>)retrieveMap("bootstrap/").get("Peers")).stream().map(x -> new MultiAddress(x)).collect(Collectors.toList());
    }

    public class Bootstrap {
        public List<MultiAddress> list() throws IOException {
            return bootstrap();
        }

        public List<MultiAddress> add(MultiAddress addr) throws IOException {
            return ((List<String>)retrieveMap("bootstrap/add?arg="+addr).get("Peers")).stream().map(x -> new MultiAddress(x)).collect(Collectors.toList());
        }

        public List<MultiAddress> rm(MultiAddress addr) throws IOException {
            return rm(addr, false);
        }

        public List<MultiAddress> rm(MultiAddress addr, boolean all) throws IOException {
            return ((List<String>)retrieveMap("bootstrap/rm?"+(all ? "all=true&":"")+"arg="+addr).get("Peers")).stream().map(x -> new MultiAddress(x)).collect(Collectors.toList());
        }
    }

    /*  ipfs swarm is a tool to manipulate the network swarm. The swarm is the
        component that opens, listens for, and maintains connections to other
        ipfs peers in the internet.
     */
    public class Swarm {
        public List<MultiAddress> peers() throws IOException {
            Map m = retrieveMap("swarm/peers?stream-channels=true");
            return ((List<Object>)m.get("Strings")).stream().map(x -> new MultiAddress((String)x)).collect(Collectors.toList());
        }

        public Map addrs() throws IOException {
            Map m = retrieveMap("swarm/addrs?stream-channels=true");
            return (Map<String, Object>)m.get("Addrs");
        }

        public Map connect(String multiAddr) throws IOException {
            Map m = retrieveMap("swarm/connect?arg="+multiAddr);
            return m;
        }

        public Map disconnect(String multiAddr) throws IOException {
            Map m = retrieveMap("swarm/disconnect?arg="+multiAddr);
            return m;
        }
    }

    public class Diag {
        public String net() throws IOException {
            return new String(retrieve("diag/net?stream-channels=true"));
        }
    }

    public Map ping(String target) throws IOException {
        return retrieveMap("ping/" + target.toString());
    }

    public Map id(String target) throws IOException {
        return retrieveMap("id/" + target.toString());
    }

    public class Stats {
        public Map bw() throws IOException {
            return retrieveMap("stats/bw");
        }
    }

    // Tools
    public String version() throws IOException {
        Map m = (Map)retrieveAndParse("version");
        return (String)m.get("Version");
    }

    public Map commands() throws IOException {
        return retrieveMap("commands");
    }

    public Map log() throws IOException {
        return retrieveMap("log/tail");
    }

    public class Config {
        public Map show() throws IOException {
            return (Map)retrieveAndParse("config/show");
        }

        public void replace(NamedStreamable file) throws IOException {
            Multipart m = new Multipart("http://" + host + ":" + port + version+"config/replace?stream-channels=true", "UTF-8");
            m.addFilePart("file", file);
            String res = m.finish();
        }

        public String get(String key) throws IOException {
            Map m = (Map)retrieveAndParse("config?arg="+key);
            return (String)m.get("Value");
        }

        public Map set(String key, String value) throws IOException {
            return retrieveMap("config?arg=" + key + "&arg=" + value);
        }
    }

    public Object update() throws IOException {
        return retrieveAndParse("update");
    }

    public class Update {
        public Object check() throws IOException {
            return retrieveAndParse("update/check");
        }

        public Object log() throws IOException {
            return retrieveAndParse("update/log");
        }
    }

    private Map retrieveMap(String path) throws IOException {
        return (Map)retrieveAndParse(path);
    }

    private Object retrieveAndParse(String path) throws IOException {
        byte[] res = retrieve(path);
        return JSONParser.parse(new String(res));
    }

    private byte[] retrieve(String path) throws IOException {
        URL target = new URL("http", host, port, version + path);
        return IPFS.get(target);
    }

    private static byte[] get(URL target) throws IOException {
        HttpURLConnection conn = (HttpURLConnection) target.openConnection();
        conn.setRequestMethod("GET");
        conn.setRequestProperty("Content-Type", "application/json");

        try {
            InputStream in = conn.getInputStream();
            ByteArrayOutputStream resp = new ByteArrayOutputStream();

            byte[] buf = new byte[4096];
            int r;
            while ((r = in.read(buf)) >= 0)
                resp.write(buf, 0, r);
            return resp.toByteArray();
        } catch (IOException e) {
            throw new RuntimeException("Trailer: " + conn.getHeaderFields().get("Trailer"), e);
        }
    }

    private InputStream retrieveStream(String path) throws IOException {
        URL target = new URL("http", host, port, version + path);
        return IPFS.getStream(target);
    }

    private static InputStream getStream(URL target) throws IOException {
        HttpURLConnection conn = (HttpURLConnection) target.openConnection();
        conn.setRequestMethod("GET");
        conn.setRequestProperty("Content-Type", "application/json");

        return conn.getInputStream();
    }

    private Map postMap(String path, byte[] body, Map<String, String> headers) throws IOException {
        URL target = new URL("http", host, port, version + path);
        return (Map) JSONParser.parse(new String(post(target, body, headers)));
    }

    private static byte[] post(URL target, byte[] body, Map<String, String> headers) throws IOException {
        HttpURLConnection conn = (HttpURLConnection) target.openConnection();
        for (String key: headers.keySet())
            conn.setRequestProperty(key, headers.get(key));
        conn.setDoOutput(true);
        conn.setRequestMethod("POST");
        conn.setRequestProperty("Content-Type", "application/json");
        OutputStream out = conn.getOutputStream();
        out.write(body);
        out.flush();
        out.close();

        InputStream in = conn.getInputStream();
        ByteArrayOutputStream resp = new ByteArrayOutputStream();
        byte[] buf = new byte[4096];
        int r;
        while ((r=in.read(buf)) >= 0)
            resp.write(buf, 0, r);
        return resp.toByteArray();
    }
}
