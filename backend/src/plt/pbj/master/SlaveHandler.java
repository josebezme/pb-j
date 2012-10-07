package plt.pbj.master;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.Socket;
import java.nio.channels.ClosedByInterruptException;

import com.google.gson.Gson;

import plt.pbj.Commands;
import plt.pbj.util.DefaultLogger;
import plt.pbj.util.Logger;

public class SlaveHandler implements Runnable, Comparable<SlaveHandler> {
	
	private static final Gson gson = new Gson();
	
	private Socket socket;
	private String name;
	
	private Logger logger = DefaultLogger.getDefaultLogger();
	private PrintWriter output;
	
	public SlaveHandler(Socket s) {
		this.socket = s;
		
		name = s.getInetAddress().getHostAddress() + ":" + Thread.currentThread().getId();
	}

	@Override
	public void run() {
		try {
			logger.log("Running slaveHandler for slave: " + getName());
			output = new PrintWriter( new OutputStreamWriter(socket.getOutputStream() ));
			BufferedReader input = new BufferedReader(new InputStreamReader( socket.getInputStream() ));
			
			output.println(Commands.Master.HI);
			output.flush();
			
			String line = input.readLine();
			
			if(Commands.Slave.HI_BACK.equals(line)) {
				logger.log("Got succesful hi from slave.");
			} else {
				logger.log("Hi failed, got:" + line);
			}
			
			// Read from slave.
			while((line = input.readLine()) != null) {
				if(Commands.CLOSE.equals(line)) {
					logger.log("Slave sent close command.");
					break;
				} else if(Commands.Slave.REPORT.equals(line)) {
					logger.log("Slave sending report.");
					String data = "";
					
					line = input.readLine();
					while(!Commands.Slave.REPORT_END.equals(line)) {
						data += line;
						line = input.readLine();
					}
					
					logger.log("Got data from slave: " + data);
				} else {
					logger.log("Got invalid command: " + line);
				}
				// Reading line
			}
				
		} catch (IOException e) {
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

}
