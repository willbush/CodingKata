using ChattingInterfaces;
using System.ServiceModel;
using System.Windows;
using System.Windows.Input;

namespace ChattingClient {

	/// <summary>
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : Window {
		public static IChattingService Server;
		private static DuplexChannelFactory<IChattingService> _channelFactory;

		public MainWindow() {
			InitializeComponent();
			_channelFactory = new DuplexChannelFactory<IChattingService>(new ClientCallback(), "ChattingServiceEndPoint");
			Server = _channelFactory.CreateChannel();
		}

		private void LoginButton_Click(object sender, RoutedEventArgs e) {
			LoginSubmit();
		}

		private void Login_KeyUp(object sender, KeyEventArgs e) {
			if (e.Key == Key.Enter) LoginSubmit();
		}

		private void LoginSubmit() {
			int returnValue = Server.Login(UserNameTextBox.Text);
			if (returnValue == 1) {
				TakeMessage("You are already logged in.", "System");
			} else if (returnValue == 0) {
				TakeMessage("You are logged in", "System");
				WelcomeLabel.Content = "Welcome " + UserNameTextBox.Text + "!";
				UserNameTextBox.IsEnabled = false;
				LoginButton.IsEnabled = false;
			}
		}

		private void SendButton_Click(object sender, RoutedEventArgs e) {
			InputSend();
		}

		private void Input_KeyUp(object sender, KeyEventArgs e) {
			if (e.Key == Key.Enter) InputSend();
		}

		private void InputSend() {
			if (InputTextBox.Text.Length > 0) {
				Server.SendMessageToAll(InputTextBox.Text, UserNameTextBox.Text);
				TakeMessage(InputTextBox.Text, "You");
				InputTextBox.Text = "";
			}
		}

		public void TakeMessage(string message, string userName) {
			TextDisplayTextBox.Text += userName + ": " + message + "\n";
			TextDisplayTextBox.ScrollToEnd();
		}
	}
}