package plt.pbj.util;
import java.lang.reflect.Method;
import java.util.*;

import plt.pbj.*;
import plt.pbj.master.Job;
import plt.pbj.master.Master;
import plt.pbj.master.SlaveHandler;

import com.google.gson.Gson;

public class PBJOp {
	private static final Gson gson = PBJ.gson;
	
	public static Object[] cleanUpArray(Object[] o) {
		for(int i = 0; i < o.length; i++) {
			Object val = o[i];
			if(val instanceof String) {
				String s = (String) val;
				if(s.startsWith("l_")) {
					o[i] = new Long(s.substring(2));
				} else if(s.startsWith("d_")) {
					o[i] = new Double(s.substring(2));
				}
			} else if(val instanceof Map) {
				cleanUpMap((Map) val);
			} else if(val instanceof List) {
				cleanUpList((List) val);
			}
		}
		
		return o;
	}
	
	public static Object cleanUpNumerical(Object o) {
		if(o instanceof String) {
			String s = (String) o;
			if(s.startsWith("l_")) {
				return new Long(s.substring(2));
			} else if(s.startsWith("d_")) {
				return new Double(s.substring(2));
			}
		} else if(o instanceof Map) {
			cleanUpMap((Map) o);
		} else if(o instanceof List) {
			cleanUpList((List) o);
		}
		
		return o;
	}
	
	private static void cleanUpMap(Map map) {
		for(Object key : map.keySet()) {
			Object val =map.get(key); 
			if(val instanceof String) {
				String s = (String) val;
				
				if(s.startsWith("l_")) {
					map.put(key, new Long(s.substring(2)));
				} else if(s.startsWith("d_")) {
					map.put(key, new Double(s.substring(2)));
				}
			} else if(val instanceof Map) {
				cleanUpMap((Map) val);
			} else if(val instanceof List) {
				cleanUpList((List) val);
			}
				
				
		}
	}
	
	private static void cleanUpList(List list) {
		for(int i = 0; i < list.size(); i++) {
			Object val = list.get(i);
			if(val instanceof String) {
				String s = (String) val;
				if(s.startsWith("l_")) {
					list.set(i, new Long(s.substring(2)));
				} else if(s.startsWith("d_")) {
					list.set(i, new Double(s.substring(2)));
				}
			} else if(val instanceof Map) {
				cleanUpMap((Map) val);
			} else if(val instanceof List) {
				cleanUpList((List) val);
			}
		}
	}

	public static Object spread(String method, Object[] args){
		Object result = null;
		synchronized (SlaveHandler.WAIT) {
			startSpread(method, args);
			outer : while(true) {
				try {
					SlaveHandler.WAIT.wait();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
				
				for(SlaveHandler handler : Master.master.getSlaveHandlers()) {
					if(handler.success()) {
						result = correctResult(handler, method);
						
						if(result != null) {
							break outer;
						}
					}
				}
			}
		}
		
		return cleanUpNumerical(result);
	}
	
	private static Object correctResult(SlaveHandler handler, String method) {
		Class<?> c = getReturnType(method);
		
		if(c.getSimpleName().equals("Long")) {
			c = String.class;
		} else if(c.getSimpleName().equals("Double")) {
			c = String.class;
		}
		
		return gson.fromJson(handler.getResult(), c);
	}
	
	public static Method getMethod(String method) {
		Class<PBJRunner> c = PBJRunner.class;
		Method m = null;
		
		for(Method poss : c.getMethods()) {
			if(poss.getName().equals(method)) {
				m = poss;
			}
		}
		
		return m;
	}
	
	public static Class<?> getReturnType(String method) {
		Method m = getMethod(method);
		return m.getReturnType();
	}
	
	public static List<Object> jam(String method, Object[] args){

		synchronized(SlaveHandler.WAIT) {
			startSpread(method, args);
			
			List<SlaveHandler> handlers = new ArrayList<SlaveHandler>(Master.master.getSlaveHandlers());
			
			while(handlers.size() > 0) {
				
				try {
					SlaveHandler.WAIT.wait();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
					
				for(Iterator<SlaveHandler> i = handlers.iterator(); i.hasNext(); ) {
					SlaveHandler sh = i.next();
					
					if(sh.success()) {
						i.remove();
					}
				}
			}
		}
		
		List<Object> objs = new ArrayList<Object>();
		for(SlaveHandler handler : Master.master.getSlaveHandlers()) {
			objs.add(correctResult(handler, method));
		}
		
		for(int i = 0; i < objs.size(); i++) {
			objs.set(i, cleanUpNumerical(objs.get(i)));
		}
		
		return objs;
	}
	
	private static void startSpread(String method, Object[] args) {
		Map<SlaveHandler, Object[]> slices = slice(args, Master.master.getSlaveHandlers());
		
		Map<SlaveHandler, Job> jobs = new HashMap<SlaveHandler, Job>();
		for(SlaveHandler handler : slices.keySet()) {
			Job job = new Job();
			job.className = "plt.pbj.PBJRunner";
			job.method = method;
			job.data = gson.toJson(slices.get(handler));
			jobs.put(handler, job);
		}
		
		Master.master.spreadJobs(jobs);
	}
	
	public static Map<SlaveHandler, Object[]> slice(Object[] objs, List<SlaveHandler> handlers) {
		Map<SlaveHandler, Object[]> out = new HashMap<SlaveHandler, Object[]>();
		
		for(SlaveHandler handler : handlers) {
			out.put(handler, Arrays.copyOf(objs, objs.length));
		}
		
		for(int i = 0; i < objs.length; i++) {
			Object o = objs[i];
			if(o instanceof Spreadable) {
				o = ((Spreadable) o).obj;
				
				if(o instanceof List) {
					List<Object> objList = (List<Object>) o;
					
					int count = (int) Math.round(objList.size() / (handlers.size() * 1.0));
					count = Math.max(count, 1); // At least one.
					
					for(int j = 0; j < handlers.size(); j++) {
						int first = j * count;
						int last =  first + count;
						last = Math.min(last, objList.size());
						
						if(first < objList.size()) {
							out.get(handlers.get(j))[i] = new ArrayList<Object>(objList.subList(first, last));
						} else {
							out.get(handlers.get(j))[i] = new ArrayList<Object>();
						}
					}
				} else if (o instanceof Map) {
					for(SlaveHandler handler : handlers) {
						out.get(handler)[i] =  new HashMap<Object, Object>();;
					}
					
					int j = 0;
					Map<Object, Object> map;
					Map<Object, Object> origMap = (Map<Object, Object>) o; 
					for(Object key : origMap.keySet()) {
						map = (Map<Object, Object>) out.get(handlers.get(j++ % handlers.size()))[i];
						map.put(key, origMap.get(key));
					}
				}
			}
		}
		
		return out;
	}
}