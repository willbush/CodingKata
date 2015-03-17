using System;
using System.ServiceModel;

namespace ChattingServer {

	internal class Program {
		public static ChattingService server;

		private static void Main(string[] args) {
			server = new ChattingService();
			using (ServiceHost host = new ServiceHost(server)) {
				host.Open();
				Console.WriteLine("Server is running...");
				Console.WriteLine("<Press Enter to Shutdown>");
				Console.ReadLine();
			}
		}
	}
}