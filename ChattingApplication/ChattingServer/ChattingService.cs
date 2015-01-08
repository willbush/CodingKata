using ChattingInterfaces;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.Serialization;
using System.ServiceModel;
using System.Text;

namespace ChattingServer {

	[ServiceBehavior(ConcurrencyMode = ConcurrencyMode.Multiple, InstanceContextMode = InstanceContextMode.Single)]
	public class ChattingService : IChattingService {
		public ConcurrentDictionary<string, ConnectedClient> _connectedClients = new ConcurrentDictionary<string, ConnectedClient>();

		public int Login(string userName) {
			int status = getDuplicateUserNameStatus(userName);
			if (status == 0) AddClient(userName);
			return status;
		}

		private void AddClient(string userName) {
			var establishedUserConnection = OperationContext.Current.GetCallbackChannel<IClient>();

			ConnectedClient newClient = new ConnectedClient();
			newClient.connection = establishedUserConnection;
			newClient.UserName = userName;
			_connectedClients.TryAdd(userName, newClient);
		}

		private int getDuplicateUserNameStatus(string userName) {
			foreach (var client in _connectedClients) {
				if (client.Key.ToLower() == userName.ToLower()) {
					return 1;
				}
			}
			return 0;
		}

		public void SendMessageToAll(string message, string userName) {
			foreach (var client in _connectedClients) {
				if (client.Key.ToLower() != userName.ToLower()) {
					client.Value.connection.GetMessage(message, userName);
				}
			}
		}
	}
}