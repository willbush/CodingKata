using ChattingInterfaces;
using System.ServiceModel;
using System.Windows;

namespace ChattingClient {

	[CallbackBehavior(ConcurrencyMode = ConcurrencyMode.Multiple)]
	public class ClientCallback : IClient {
		private MainWindow mainWindow = (MainWindow)Application.Current.MainWindow;

		public void GetMessage(string message, string userName) {
			mainWindow.TakeMessage(message, userName);
		}

		public void GetUpdate(int value, string userName) {
			switch (value) {
				case 0: {
						mainWindow.AddUserToList(userName);
						break;
					}
				case 1: {
						mainWindow.RemoveUserFromList(userName);
						break;
					}
			}
		}
	}
}