﻿using System.Collections.Generic;
using System.ServiceModel;

namespace ChattingInterfaces {

	// NOTE: You can use the "Rename" command on the "Refactor" menu to change the interface name "IChattingService" in both code and config file together.
	[ServiceContract(CallbackContract = typeof(IClient))]
	public interface IChattingService {

		[OperationContract]
		int Login(string userName);

		[OperationContract]
		void Logout();

		[OperationContract]
		void SendMessageToAll(string message, string senderUserName);

		[OperationContract]
		List<string> GetCurrentUsers();
	}
}