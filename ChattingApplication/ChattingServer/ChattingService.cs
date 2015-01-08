using ChattingInterfaces;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.ServiceModel;

namespace ChattingServer {

	[ServiceBehavior(ConcurrencyMode = ConcurrencyMode.Multiple, InstanceContextMode = InstanceContextMode.Single)]
	public class ChattingService : IChattingService {
		public ConcurrentDictionary<string, ConnectedClient> connectedClients = new ConcurrentDictionary<string, ConnectedClient>();

		public int Login(string userName) {
			int status = GetDuplicateUserNameStatus(userName);
			if (status == 0) AddClient(userName);
			return status;
		}

		private void AddClient(string userName) {
			var establishedUserConnection = OperationContext.Current.GetCallbackChannel<IClient>();

			ConnectedClient newClient = new ConnectedClient();
			newClient.connection = establishedUserConnection;
			newClient.UserName = userName;
			connectedClients.TryAdd(userName, newClient);
			UpdateHelper(0, newClient.UserName);

			Console.ForegroundColor = ConsoleColor.Green;
			Console.WriteLine("Client logged in: {0} at {1}", newClient.UserName, System.DateTime.Now);
			Console.ResetColor();
		}

		private int GetDuplicateUserNameStatus(string userName) {
			foreach (var client in connectedClients) {
				if (client.Key.ToLower() == userName.ToLower()) {
					return 1;
				}
			}
			return 0;
		}

		public void SendMessageToAll(string message, string userName) {
			foreach (var client in connectedClients) {
				if (client.Key.ToLower() != userName.ToLower()) {
					client.Value.connection.GetMessage(message, userName);
				}
			}
		}

		public void Logout() {
			ConnectedClient client = GetMyClient();
			if (client != null) {
				ConnectedClient removedClient;
				connectedClients.TryRemove(client.UserName, out removedClient);
				UpdateHelper(1, removedClient.UserName);

				Console.ForegroundColor = ConsoleColor.Cyan;
				Console.WriteLine("Client logoff: {0} at {1}", removedClient.UserName, System.DateTime.Now);
				Console.ResetColor();
			}
		}

		public ConnectedClient GetMyClient() {
			var establishedUserConnection = OperationContext.Current.GetCallbackChannel<IClient>();
			foreach (var client in connectedClients) {
				if (client.Value.connection == establishedUserConnection) {
					return client.Value;
				}
			}
			return null;
		}

		private void UpdateHelper(int value, string userName) {
			foreach (var client in connectedClients) {
				if (client.Value.UserName.ToLower() != userName.ToLower()) {
					client.Value.connection.GetUpdate(value, userName);
				}
			}
		}

		public List<string> GetCurrentUsers() {
			List<string> listOfUsers = new List<string>();
			foreach (var client in connectedClients) {
				listOfUsers.Add(client.Value.UserName);
			}
			return listOfUsers;
		}
	}
}