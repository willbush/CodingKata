package com.thecherno.chernochat.server;

import java.net.InetAddress;

public class ServerClient {

	private String name;
	private InetAddress address;
	private int port;
	private final int ID;
	public int attempt = 0;

	public ServerClient(String name, InetAddress address, int port, final int ID) {
		this.name = name;
		this.address = address;
		this.port = port;
		this.ID = ID;
	}

	public String getName() {
		return name;
	}

	public InetAddress getAddress() {
		return address;
	}

	public int getPort() {
		return port;
	}

	public int getID() {
		return ID;
	}

}
