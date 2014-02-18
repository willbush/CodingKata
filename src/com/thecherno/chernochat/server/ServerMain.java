package com.thecherno.chernochat.server;

public class ServerMain {

	private int port;
	private Server server;

	public ServerMain(int port) {
		this.setPort(port);
		setServer(new Server(port));
	}

	public static void main(String[] args) {
		int port;
		if (args.length != 1) {
			System.out.println("Usage: java -jar ChernoChatServer [port]");
			return;
		}
		port = Integer.parseInt(args[0]);
		new ServerMain(port);
	}

	public int getPort() {
		return port;
	}

	public void setPort(int port) {
		this.port = port;
	}

	public Server getServer() {
		return server;
	}

	public void setServer(Server server) {
		this.server = server;
	}

}
