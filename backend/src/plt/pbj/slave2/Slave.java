package plt.pbj.slave2;

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
import java.util.LinkedList;

import com.google.gson.Gson;

import plt.pbj.Commands;
import plt.pbj.master.Job;
import plt.pbj.master.Master;
import plt.pbj.util.DefaultLogger;
import plt.pbj.util.Logger;

public class Slave{// implements Runnable {

	private Gson gson = new Gson();
	private Logger logger = DefaultLogger.getDefaultLogger();
	
	private JobRunner jobRunner;
	private String ipAddress;
	private int port;
	private PrintWriter output;
	
	public static void main( String[] args ){
//		Slave S = args.length < 2 ? 
//				new Slave( args[0] ) : 
//					new Slave( args[0], Integer.parseInt(args[1]) ); 
//		Slave s = new Slave(Integer.parseInt(args[0]));
		Slave s = new Slave();
		s.connect();
	}
	
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
	
	public Slave(int port) {
		this("127.0.0.1", port);
	}

//	@Override
	public void connect() {
		try {
			logger.log(	"slave- Opening socket to ip:" + ipAddress + 
						" on port: " + port);
			
			Socket socket = new Socket(ipAddress, port);
			
			logger.log("slave- Socket to master opened...");
			output = new PrintWriter( new OutputStreamWriter(socket.getOutputStream() ));;
			BufferedReader input = new BufferedReader(new InputStreamReader( socket.getInputStream() ));
			
			String line = input.readLine();
			
			if(Commands.Master.HI.equals(line)) {
				logger.log("slave: Got hi from master... sending hi back.");
				output.println(Commands.Slave.HI_BACK);
				output.flush();
			} else {
				logger.log("slave: Got bad starting command: " + line);
			}
			
			// Handle input from slave.
			while((line = input.readLine()) != null) {
				logger.log("slave: Got from master: " + line);
				
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
//		logger.log("Reporting results: " + result);
		output.println(Commands.Slave.REPORT);
		output.println(jobRunner.result);
		output.println(Commands.Slave.REPORT_END);
		output.flush();
	}
	
	public void performReturn(String result) {
		logger.log("Returning: " + result);
		output.println(Commands.Slave.RETURN);
		output.println(jobRunner.result);
		output.println(Commands.Slave.RETURN_END);
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
				Method m = c.getMethod(job.method, LinkedList.class);
				LinkedList args = gson.fromJson(job.data, LinkedList.class);
				Object returned =  m.invoke(null, args);
//				result = (String) m.invoke(null);//, job.data);
				complete = true;
				result = gson.toJson(returned, m.getReturnType());
//				result = gson.toJson(result);
				if(!abort) {
					performReturn(result);
				}
				
			} catch (Exception e) {
				System.out.println(e.getClass());
				e.printStackTrace();
			}
		}
	}

}
