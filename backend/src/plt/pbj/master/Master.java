package plt.pbj.master;

import java.io.IOException;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import plt.pbj.slave.SlaveAddress;
import plt.pbj.util.DefaultLogger;
import plt.pbj.util.Logger;

public class Master implements Runnable {
	
	public static int DEFAULT_PORT = 9001;
	
	public static Master master;
	
	private List<SlaveHandler> slaveHandlers = new ArrayList<SlaveHandler>();
	private Set<MasterObserver> observers = new HashSet<MasterObserver>();

	private Logger logger = DefaultLogger.getDefaultLogger();

	private List<SlaveAddress> addresses;
	
	{
		master = this;
	}
	
	public Master(List<SlaveAddress> addresses) {
		this.addresses = addresses;
	}
	
	@Override
	public void run() {
			
		Socket socket;
		for(SlaveAddress address : addresses) {
			try {
				logger.log("Opening connection to slave: " + address);
				socket = new Socket(address.ip, address.port);
				logger.log("Adding slave to pool...");
				SlaveHandler slaveRunner = new SlaveHandler(socket);
				
				synchronized (slaveRunner) {
					
					new Thread(slaveRunner).start();
					slaveHandlers.add(slaveRunner);
					
					try {
						slaveRunner.wait();
					} catch (InterruptedException e) {
						e.printStackTrace();
					}

				}
			} catch (UnknownHostException e1) {
				System.out.println("Unknown host for: " + address);
				e1.printStackTrace();
				
			} catch (IOException e1) {
				System.out.println("IOException for: " + address);
				e1.printStackTrace();
			}
		}
		
		if(slaveHandlers.size() > 0) {
			logger.log("Initialized with slaves: " + slaveHandlers);
			// Notify that we're done.
			synchronized (this) {
				this.notifyAll();
			}
		} else {
			System.err.println("No slaves connected.");
			System.exit(1);
		}
			
		
			
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

	public List<SlaveHandler> getSlaveHandlers() {
		return slaveHandlers;
	}

	/**
	 * 
	 * @param jobs <Slave Name, Job>
	 */
	public void spreadJobs(Map<SlaveHandler, Job> jobs) {
		for(SlaveHandler handler : slaveHandlers) {
			handler.sendJob(jobs.get(handler));
		}
	}
	
	public void close() {
		for(SlaveHandler handler : slaveHandlers) {
			handler.close();
		}
	}
}
