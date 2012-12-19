package plt.pbj.slave;

public class SlaveAddress {
	
	public String ip;
	public int port;
	
	public SlaveAddress(String ip, int port) {
		this.ip = ip;
		this.port = port;
	}
	
	
	@Override
	public String toString() {
		return ip + ":" + port;
	}

}
