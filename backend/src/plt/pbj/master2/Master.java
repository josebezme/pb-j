package plt.pbj.master2;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.AbstractListModel;

import plt.pbj.master.Job;
import plt.pbj.master2.SlaveHandler;
import plt.pbj.slave.Slave;
import plt.pbj.test.sample.Test;
import plt.pbj.util.DefaultLogger;
import plt.pbj.util.Logger;

public class Master implements Runnable {
	
	public static int DEFAULT_PORT = 35000;
	
	private int port, slavesConnected = 0, slavesDone = 0, totalSlaves = -1;
	private String className;
	private ServerSocket socket;
	private boolean stopping;
	
	Logger logger = DefaultLogger.getDefaultLogger();
	
	
	private List<SlaveHandler> slaveHandlers = new ArrayList<SlaveHandler>();
//	private Set<MasterObserver> observers = new HashSet<MasterObserver>();

	public Map<String, Object> returns;
	

	
	/**
	 * 
	 * @param args [0] is the class while the rest is a list of slaves ip:port
	 */
	public static void main (String[] args){
//		String cn = args[0];
//		List<SlaveHandler> slaveHandlers = args.length > 0 ?
//				new ArrayList(args.length - 1) : null;
//		List<SlaveHandler> slaveHandlers = new ArrayList();
		
//		for(int i = 1; i < args.length; i++){
//			String[] ipPort = args[i].split(":");
//			slaveHandlers.add(new SlaveHandler(new Socket(ipPort[0], Integer.parseInt(ipPort[1]))));
//		}
//			if(event.getSource() == addSlave) {
//				new Thread(new Slave()).start();
		
		
		
//		Master m = new Master(cn, args.length - 1);
		Master m = new Master("plt.pbj.test.sample.TestJamSpread", 3);
		new Thread(m).start();
	}
	
	public void run(){
		this.slaveHandlers = createSlaveHandlers();
		returns = new HashMap<String, Object>();
		for(SlaveHandler sh : slaveHandlers){
			returns.put(sh.getName(), new ArrayList<Collection<Object>>());
		}
		runProgram();
		stop();
	}
	
	
	
	public Master() {
		this(DEFAULT_PORT);
	}
	
	public Master(int port) {
		this.port = port;
	}
	
	public Master(String className, int totalSlaves){
		this.totalSlaves = totalSlaves;
		this.className = className;
	}
	
	public void runProgram(){
		try {
			Class c = Class.forName(className);
			@SuppressWarnings("unchecked")
			Method m = c.getMethod("master", Master.class, List.class, List.class);//, String.class);
			Object[] args = new String[0];
			String result = (String) m.invoke(null, this, slaveHandlers, Arrays.asList(args));
		} catch (Exception e) {
			System.out.println(e.getClass());
			e.printStackTrace();
		} 
	}
	
	public List<SlaveHandler> createSlaveHandlers( ){
		Set<SlaveHandler> sh = new HashSet<SlaveHandler>();
		try {
			logger.log("M: Binding socket to port: " + port);
			socket = new ServerSocket(DEFAULT_PORT);
			
			while(slavesConnected < totalSlaves) {
				logger.log("M: Accepting on socket...");
				Socket slaveSocket = socket.accept();			
				logger.log("M: Adding slave to pool...");
				logger.log("M: " + slavesConnected);
				SlaveHandler slaveRunner = new SlaveHandler(slaveSocket, this);
//				synchronized (slaveRunner) {
					new Thread(slaveRunner).start();
					sh.add(slaveRunner);
					int temp = slavesConnected;
					while(slavesConnected == temp) {
						try {
							Thread.sleep(1000);
							logger.log("M: " + slavesConnected);
						} catch (InterruptedException e) {
							logger.log("M: Notified of Connection");
						}
					}
					logger.log("M: " + slavesConnected);
//					slaveRunner.run();
//					try {
//						slaveRunner.wait();		
//						logger.log("M: ?");
//					} catch (InterruptedException e) {
//						e.printStackTrace();
//					}
//				}
			}
			
		} catch (IOException e) {
//			if(!stopping) {
				e.printStackTrace();
//			}
		}
		logger.log("M: Done collecting Slaves");

		List<SlaveHandler> shList = new ArrayList<SlaveHandler>();
		shList.addAll( Arrays.asList( sh.toArray(new SlaveHandler[0])));
		Collections.sort(shList);
		return shList;
	}
	

	
	public void stop() {
		try {
			stopping = true;
			for(SlaveHandler sh : slaveHandlers)
				sh.close();
			socket.close();
			logger.log("Closing socket on master...");
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		slaveHandlers.clear();
	}
	
//	private void notifySlaveChange() {
//		for(MasterObserver observer : observers) {
//			observer.notifySlaveChange();
//		}
//	}
//	
//	private void notifySlaveDone() {
//		for(MasterObserver observer : observers) {
//			observer.notifySlaveDone();
//		}
//	}
//	
//	public void setLogger(Logger logger) {
//		this.logger = logger;
//	}
//	
//	public void removeLogger() {
//		logger = DefaultLogger.getDefaultLogger();
//	}
//	
//	public void addObserver(MasterObserver observer) {
//		observers.add(observer);
//	}
//	
//	public void removeObserver(MasterObserver observer) {
//		observers.remove(observer);
//	}
//	
//	public interface MasterObserver {
//		void notifySlaveChange();
//		void notifySlaveDone();
//	}

	public List<SlaveHandler> getSlaveHandlers() {
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

	public synchronized void incSlavesDone() {
		slavesDone++;
	}

	public synchronized void resetSlavesDone() {
		slavesDone = 0;
	}
	
	public synchronized void incSlavesConnected() {
		slavesConnected++;
		logger.log("M: slaves ++");
		logger.log("M: slavesnow " + slavesConnected);
	}
	
	public int getSlavesDone() {
		return slavesDone;
	}

//	public Map<String, List<Collection<Object>>> getReturns() {
//		return returns;
//	}
	
	public Map<String, Object> getReturns() {
		return returns;
	}
	
	public void give(String gotFrom, Object object) {
		incSlavesDone();
		returns.put(gotFrom, object);
//		returns.put(gotFrom, returned);
	}

//	public Map<String, List<Collection<Object>>> getJamReturns() {
//		return jamReturns;
//	}
}
