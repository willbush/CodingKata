using ChattingInterfaces;
using System.ServiceModel;
using System.Windows;

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
	}
}