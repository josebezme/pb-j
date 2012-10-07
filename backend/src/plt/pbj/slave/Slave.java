package plt.pbj.slave;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.Socket;
import java.net.UnknownHostException;
import java.nio.channels.ClosedByInterruptException;

import com.google.gson.Gson;

import plt.pbj.Commands;
import plt.pbj.master.Job;
import plt.pbj.master.Master;
import plt.pbj.util.DefaultLogger;
import plt.pbj.util.Logger;

public class Slave implements Runnable {

	private Gson gson = new Gson();
	private Logger logger = DefaultLogger.getDefaultLogger();
	
	private JobRunner jobRunner;
	private String ipAddress;
	private int port;
	private PrintWriter output;
	
	
	public Slave() {
		this("127.0.0.1", Master.DEFAULT_PORT);
	}

	public Slave(String ipAddress) {
		this(ipAddress, Master.DEFAULT_PORT);
	}
	
	public Slave(String ipAddress, int port) {
		this.ipAddress = ipAddress;
		this.port = port;
	}
	
	@Override
	public void run() {
		try {
			logger.log(	"Opening socket to ip:" + ipAddress + 
						" on port: " + port);
			
			Socket socket = new Socket(ipAddress, port);
			
			logger.log("Socket to master opened...");
			output = new PrintWriter( new OutputStreamWriter(socket.getOutputStream() ));;
			BufferedReader input = new BufferedReader(new InputStreamReader( socket.getInputStream() ));
			
			String line = input.readLine();
			
			if(Commands.Master.HI.equals(line)) {
				logger.log("Got hi from master... sending hi back.");
				output.println(Commands.Slave.HI_BACK);
				output.flush();
			} else {
				logger.log("Got bad starting command: " + line);
			}
			
			// Handle input from slave.
			while((line = input.readLine()) != null) {
				logger.log("Got from master: " + line);
				
				if(Commands.Master.JOB.equals(line)) {
					String jobData = "";
					
					line = input.readLine();
					while(!Commands.Master.JOB_END.equals(line)) {
						jobData += line;
						line = input.readLine();
					}
					
					logger.log("Got job data:" + jobData);
					Job job = gson.fromJson(jobData, Job.class);
					jobRunner = new JobRunner(job);
					jobRunner.start();
					
				} else if(Commands.Master.ABORT.equals(line)) {
					jobRunner.abort = true;
					jobRunner.interrupt();
					jobRunner = null;
				} else if(Commands.CLOSE.equals(line)) {
					break;
				}
				
				logger.log("Waiting for next line...");
			}
					
			logger.log("Finishing slave socket.");
			
		} catch (UnknownHostException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void reportResults(String result) {
		logger.log("Reporting results: " + result);
		output.println(Commands.Slave.REPORT);
		output.println(jobRunner.result);
		output.println(Commands.Slave.REPORT_END);
		output.flush();
	}
	
	public class JobRunner extends Thread {
		private Job job;
		
		public String result;
		public boolean complete;
		public boolean abort;
		
		public JobRunner(Job job) {
			this.job = job;
		}

		@SuppressWarnings({ "unchecked", "rawtypes" })
		@Override
		public void run() {
			try {
				Class c = Class.forName(job.className);
				Method m = c.getMethod(job.method, String.class);
				
				result = (String) m.invoke(null, job.data);
				complete = true;
				
				if(!abort) {
					reportResults(result);
				}
				
			} catch (ClassNotFoundException e) {
				e.printStackTrace();
			} catch (SecurityException e) {
				e.printStackTrace();
			} catch (NoSuchMethodException e) {
				e.printStackTrace();
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
			} catch (IllegalAccessException e) {
				e.printStackTrace();
			} catch (InvocationTargetException e) {
				e.printStackTrace();
			}
		}
	}

}
