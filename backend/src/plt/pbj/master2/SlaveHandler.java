package plt.pbj.master2;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.nio.channels.ClosedByInterruptException;
import java.util.Collection;

import com.google.gson.Gson;

import plt.pbj.master.Job;
import plt.pbj.Commands;
import plt.pbj.util.DefaultLogger;
import plt.pbj.util.Logger;

public class SlaveHandler implements Runnable, Comparable<SlaveHandler> {
	
	private static final Gson gson = new Gson();
	
	private Socket socket;
	private String name;
	
	private Logger logger = DefaultLogger.getDefaultLogger();
	private PrintWriter output;

	private Master master;
	
	public SlaveHandler(Socket s, Master master) {
		this.socket = s;
		this.master = master;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void run() {
		try {
			name = socket.getInetAddress().getHostAddress() + ":" + Thread.currentThread().getId();
			
			synchronized(this) {
				this.notifyAll();
			}
			
			
//			logger.log("Running slaveHandler for slave: " + getName());
			output = new PrintWriter( new OutputStreamWriter(socket.getOutputStream() ));
			BufferedReader input = new BufferedReader(new InputStreamReader( socket.getInputStream() ));
			
			output.println(Commands.Master.HI);
			output.flush();
			
			String line = input.readLine();
			
			if(Commands.Slave.HI_BACK.equals(line)) {
				logger.log("sh: Got succesful hi from slave. ");
				synchronized(this) {
					master.incSlavesConnected();
					logger.log("sh: Slave++. ");
					this.notifyAll();
				}
			} else {
				logger.log("sh: Hi failed, got:" + line);
			}
			
			// Read from slave.
			while( !socket.isClosed() && (line = input.readLine()) != null) {
				if(Commands.CLOSE.equals(line)) {
					logger.log("sh: Slave sent close command.");
					break;
				} else if(Commands.Slave.REPORT.equals(line)) {
					logger.log("sh: Slave sending report.");
					String data = "";
					
					line = input.readLine();
					while(!Commands.Slave.REPORT_END.equals(line)) {
						data += line;
						line = input.readLine();
					}
					
					logger.log("sh: Got data from slave: " + data);
					
				} else if( Commands.Slave.RETURN.equals(line)){
					logger.log("sh: slave returning.");
					String data = "";
					line = input.readLine();
					while(!Commands.Slave.RETURN_END.equals(line)) {
						data += line;
						line = input.readLine();
					}
					master.give(name, gson.fromJson(data, Object.class));
//					this.notifyAll();
//					try{
//						master.give(name, gson.fromJson(data, Object.class));
//					}catch (Exception e){
//						logger.log("sh: not jamming");
//					}
				} else {
					logger.log("Got invalid command: " + line);
				}
				// Reading line
			}
				
		}catch(java.net.SocketException e){
			//business as usual
		}catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public String getName() {
		return name;
	}

	@Override
	public int compareTo(SlaveHandler other) {
		return name.compareTo(other.getName());
	}

	public void sendJob(Job job) {
		logger.log("Sending job to slave:" + getName());
		output.println(Commands.Master.JOB);
		output.println(gson.toJson(job));
		output.println(Commands.Master.JOB_END);
		output.flush();
	}

	public void close() {
		output.println(Commands.Master.ABORT);
		output.flush();
		try {
			socket.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

}
