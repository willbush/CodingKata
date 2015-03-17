using System.ServiceModel;

namespace ChattingInterfaces {

	public interface IClient {

		[OperationContract]
		void GetMessage(string message, string senderUserName);

		[OperationContract]
		void GetUpdate(int value, string userName);
	}
}