package com.thecherno.chernochat;

import java.awt.EventQueue;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.UIManager;
import javax.swing.border.EmptyBorder;

public class Login extends JFrame {

	private static final long serialVersionUID = 1L;
	private JPanel contentPane;
	private JTextField txtName;
	private JTextField txtAddress;
	private JLabel lblIpAdress;
	private JTextField txtPort;
	private JLabel lblPort;
	private JLabel lblIPdesc;
	private JLabel lblPortDesc;

	public Login() {

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception e) {
			e.printStackTrace();
		}

		setResizable(false);
		setTitle("Login");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setSize(300, 350);
		setLocationRelativeTo(null);
		contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(5, 5, 5, 5));
		setContentPane(contentPane);
		contentPane.setLayout(null);

		txtName = new JTextField();
		txtName.setBounds(64, 51, 165, 25);
		contentPane.add(txtName);
		txtName.setColumns(10);

		JLabel lblName = new JLabel("Name");
		lblName.setBounds(124, 24, 46, 15);
		contentPane.add(lblName);

		txtAddress = new JTextField();
		txtAddress.setBounds(64, 113, 165, 25);
		contentPane.add(txtAddress);
		txtAddress.setColumns(10);

		lblIpAdress = new JLabel("IP Adress:");
		lblIpAdress.setBounds(107, 96, 80, 15);
		contentPane.add(lblIpAdress);

		txtPort = new JTextField();
		txtPort.setColumns(10);
		txtPort.setBounds(64, 197, 165, 25);
		contentPane.add(txtPort);

		lblPort = new JLabel("Port:");
		lblPort.setBounds(127, 177, 40, 15);
		contentPane.add(lblPort);

		lblIPdesc = new JLabel("(e.g. 192.168.0.1)");
		lblIPdesc.setBounds(86, 139, 121, 15);
		contentPane.add(lblIPdesc);

		lblPortDesc = new JLabel("(e.g. 8192)");
		lblPortDesc.setBounds(110, 222, 74, 15);
		contentPane.add(lblPortDesc);

		JButton btnNewButton = new JButton("Login");
		btnNewButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				String name = txtName.getText();
				String address = txtAddress.getText();
				int port = Integer.parseInt(txtPort.getText());
				login(name, address, port);
			}
		});
		btnNewButton.setBounds(86, 267, 117, 25);
		contentPane.add(btnNewButton);
	}

	private void login(String name, String address, int port) {
		dispose();
		new ClientWindow(name, address, port);
	}

	public static void main(String[] args) {
		EventQueue.invokeLater(new Runnable() {
			public void run() {
				try {
					Login frame = new Login();
					frame.setVisible(true);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		});
	}
}
