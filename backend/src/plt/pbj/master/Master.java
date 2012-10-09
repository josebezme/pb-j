package plt.pbj.master;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import plt.pbj.util.DefaultLogger;
import plt.pbj.util.Logger;

public class Master implements Runnable {
	
	public static int DEFAULT_PORT = 35000;
	
	private int port;
	
	private Set<SlaveHandler> slaveHandlers = new HashSet<SlaveHandler>();
	private Set<MasterObserver> observers = new HashSet<MasterObserver>();

	private ServerSocket socket;
	private Logger logger = DefaultLogger.getDefaultLogger();
	private boolean stopping;
	
	public Master() {
		this(DEFAULT_PORT);
	}
	
	public Master(int port) {
		this.port = port;
	}
	
	@Override
	public void run() {
		try {
			logger.log("Binding socket to port: " + port);
			socket = new ServerSocket(port);
			
			while(true) {
				logger.log("Accepting on socket...");
				Socket slaveSocket = socket.accept();
				
				logger.log("Adding slave to pool...");
				SlaveHandler slaveRunner = new SlaveHandler(slaveSocket);
				
				synchronized (slaveRunner) {
					
					new Thread(slaveRunner).start();
					slaveHandlers.add(slaveRunner);
					
					try {
						slaveRunner.wait();
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
				
				notifySlaveChange();
			}
			
		} catch (IOException e) {
			if(!stopping) {
				e.printStackTrace();
			}
		}
	}
	
	public void stop() {
		try {
			stopping = true;
			socket.close();
			logger.log("Closing socket on master...");
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		slaveHandlers.clear();
	}
	
	private void notifySlaveChange() {
		for(MasterObserver observer : observers) {
			observer.notifySlaveChange();
		}
	}
	
	public void setLogger(Logger logger) {
		this.logger = logger;
	}
	
	public void removeLogger() {
		logger = DefaultLogger.getDefaultLogger();
	}
	
	public void addObserver(MasterObserver observer) {
		observers.add(observer);
	}
	
	public void removeObserver(MasterObserver observer) {
		observers.remove(observer);
	}
	
	public interface MasterObserver {
		void notifySlaveChange();
	}

	public Set<SlaveHandler> getSlaveHandlers() {
		return slaveHandlers;
	}

	/**
	 * 
	 * @param jobs <Slave Name, Job>
	 */
	public void spreadJobs(Map<String, Job> jobs) {
		Job job = null;
		for(SlaveHandler handler : slaveHandlers) {
			if((job = jobs.get(handler.getName())) != null) {
				handler.sendJob(job);
			}
		}
	}
}
