package org.ipfs.api;

import java.io.*;
import java.net.*;
import java.util.*;

public class Multipart {
    private final String boundary;
    private static final String LINE_FEED = "\r\n";
    private HttpURLConnection httpConn;
    private String charset;
    private OutputStream out;
    private PrintWriter writer;

    public Multipart(String requestURL, String charset) throws IOException {
        this.charset = charset;

        boundary = createBoundary();

        URL url = new URL(requestURL);
        httpConn = (HttpURLConnection) url.openConnection();
        httpConn.setUseCaches(false);
        httpConn.setDoOutput(true);
        httpConn.setDoInput(true);
        httpConn.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);
        httpConn.setRequestProperty("User-Agent", "Java IPFS CLient");
        out = httpConn.getOutputStream();
        writer = new PrintWriter(new OutputStreamWriter(out, charset), true);
    }

    public static String createBoundary() {
        Random r = new Random();
        String allowed = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
        StringBuilder b = new StringBuilder();
        for (int i=0; i < 32; i++)
            b.append(allowed.charAt(r.nextInt(allowed.length())));
        return b.toString();
    }

    public void addFormField(String name, String value) {
        writer.append("--" + boundary).append(LINE_FEED);
        writer.append("Content-Disposition: form-data; name=\"" + name + "\"")
                .append(LINE_FEED);
        writer.append("Content-Type: text/plain; charset=" + charset).append(
                LINE_FEED);
        writer.append(LINE_FEED);
        writer.append(value).append(LINE_FEED);
        writer.flush();
    }

    public void addFilePart(String fieldName, NamedStreamable uploadFile) throws IOException {
        Optional<String> fileName = uploadFile.getName();
        writer.append("--" + boundary).append(LINE_FEED);
        if (!fileName.isPresent())
            writer.append("Content-Disposition: file; name=\"" + fieldName + "\";").append(LINE_FEED);
        else
            writer.append("Content-Disposition: file; name=\"" + fieldName + "\"; filename=\"" + fileName.get() + "\"").append(LINE_FEED);
        writer.append("Content-Type: application/octet-stream").append(LINE_FEED);
        writer.append("Content-Transfer-Encoding: binary").append(LINE_FEED);
        writer.append(LINE_FEED);
        writer.flush();

        InputStream inputStream = uploadFile.getInputStream();
        byte[] buffer = new byte[4096];
        int r;
        while ((r = inputStream.read(buffer)) != -1)
            out.write(buffer, 0, r);
        out.flush();
        inputStream.close();

        writer.append(LINE_FEED);
        writer.flush();
    }

    public void addHeaderField(String name, String value) {
        writer.append(name + ": " + value).append(LINE_FEED);
        writer.flush();
    }

    public String finish() throws IOException {
        StringBuilder b = new StringBuilder();

        writer.append("--" + boundary + "--").append(LINE_FEED);
        writer.close();

        int status = httpConn.getResponseCode();
        if (status == HttpURLConnection.HTTP_OK) {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                    httpConn.getInputStream()));
            String line;
            while ((line = reader.readLine()) != null) {
                b.append(line);
            }
            reader.close();
            httpConn.disconnect();
        } else {
            try {
                BufferedReader reader = new BufferedReader(new InputStreamReader(
                        httpConn.getInputStream()));
                String line;
                while ((line = reader.readLine()) != null) {
                    b.append(line);
                }
                reader.close();
            } catch (Throwable t) {}
            throw new IOException("Server returned status: " + status + " with body: "+b.toString() + " and Trailer header: "+httpConn.getHeaderFields().get("Trailer"));
        }

        return b.toString();
    }
}
