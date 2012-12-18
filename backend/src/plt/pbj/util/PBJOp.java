package plt.pbj.util;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

import plt.pbj.*;
import plt.pbj.master2.Master;
import plt.pbj.master2.SlaveHandler;
import plt.pbj.master.Job;
import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

public class PBJOp {
	private static final Gson g = new Gson();
	
	@SuppressWarnings("unchecked")
	public static void main( String[] args){
		LinkedList<Object> inNormal = new LinkedList<Object>();
		Map<String, List<Collection<Object>>> slices;
		ArrayList<Collection<Object>> inSliced = new ArrayList<Collection<Object>>();
		Object[] s = {
				(Object) new Integer(80),
				(Object) new Double(2.2),
				(Object) new Double(4.2),
				(Object) "Hey"
		};
		Object sliceone = (Object) Arrays.asList(s);
		
		inSliced.add((Collection<Object>) sliceone);
		inNormal.push(new Integer(5));
		Object[] o = {
				(Object) new Integer(8),
				(Object) new Double(2.2)
		};
		inNormal.push(Arrays.asList(o));
		inNormal.push("hey");
		String[] sh = {
				"a",
				"b",
				"c"
		};
		slices = slice(Arrays.asList(sh), inSliced);
		for( String str : sh){
			System.out.println(makeJsons(inNormal, slices.get(str)));
		}
//		System.out.println(stdJam(slices));
	}
	
	public static Object jamSliceSpread( 
			Master master, String className, 
			String jamMethod, List< Object > jamActuals,
			String method, List< Object > inNormal, List<Collection<Object>> inSliced){

		sliceSpread( master, className, method, inNormal, inSliced);
		while(true){
			try {
				Thread.sleep(100);
				if(master.getSlavesDone() == master.getSlaveHandlers().size()){
					return doJam(className, jamMethod, jamActuals, master.getReturns());					
				}
			} catch (InterruptedException e) {
				if(master.getSlavesDone() == master.getSlaveHandlers().size()){
					return doJam(className, jamMethod, jamActuals, master.getReturns());					
				}
			}
		}
		
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	private static Object doJam(String className, String jamMethod,
			List<Object> jamActuals, Map<String, Object> returns) {
		Object result = null;
		try {
//			return stdJam(returns);
			if (jamMethod == "stdJam") return stdJam(returns);
			Class c = Class.forName(className);
			Method m = c.getMethod(jamMethod, Map.class, LinkedList.class);
			result = m.invoke(null, returns, new LinkedList((Collection)jamActuals));
		} catch (Exception e) {
			e.printStackTrace();
			System.out.print("Error type: " + e.getClass());
		} 
		return result;
	}

	private static List<Object> stdJam(Map<String, Object> returns) {
		List<Object> out = new ArrayList<Object>(); 
		List<String> keys = new ArrayList<String>(returns.keySet());
		Collections.sort(keys);
		for( String s : keys ){
			int i = 0;
			out.add(returns.get(s));
			i++;
		}
//		System.out.println("jammed");
		return out;
	}

private static List<String> toStringList(List<SlaveHandler> sh){
	List<String> shString = new ArrayList<String>(sh.size());
	for(SlaveHandler handler : sh)
		shString.add(handler.getName());
	return shString;
}

	public static void sliceSpread( Master master, String className, String method, List< Object > inNormal, List<Collection<Object>> inSliced){
		List<SlaveHandler> sh = master.getSlaveHandlers();
		Map<String, Job> jobs = new HashMap<String, Job>();
		Map<String, List<Collection<Object>>> slices;		
		
		master.resetSlavesDone();
		
		slices = slice(toStringList(sh), inSliced);
		
		for(SlaveHandler handler : master.getSlaveHandlers()) { 
	       Job job = new Job();
	       job.className = className;
	       job.method = method;
	       job.data = makeJsons( inNormal, slices.get(handler.getName()));
	       jobs.put( handler.getName(), job ); 
		} 
		master.spreadJobs(jobs);
	}
	
	
	private static String makeJsons(List<Object> inNormal, List<Collection<Object>> list) {
		LinkedList<Object> l = new LinkedList<Object>();
		l.addAll(inNormal);
		l.addAll(list);
		return g.toJson(l);
	}

	private static  Map<String, List<Collection<Object>>> slice( 
			List< String > sh, List< Collection<Object> > objects) {
		Map< String, List< Collection<Object> > > out = new HashMap< String, List< Collection<Object> > >();
		
		for(String s : sh)
			out.put(s, new ArrayList< Collection<Object> >());
		
		for( int i = 0; i < objects.size(); i++){
			System.out.println(objects.get(i).getClass());
			if (objects.get(i) instanceof List){
//				System.out.println("ex");
				@SuppressWarnings("unchecked")
				List<Object> l = (List<Object>) objects.get(i);
				int sectionSize = l.size() / sh.size();
				int j = 0;
				for(String s : sh) {
					if (j == sh.size() - 1)
						out.get(s).add( l.subList(sectionSize * j, l.size()));
					else
						out.get(s).add( l.subList(sectionSize * j, sectionSize * (j + 1)));
					j++;
				}
			}
		}
//		System.out.println(out.toString());
		return out;
//		if (ob instanceof Map){
//			//not yet
//		}
//		return null;
	}
	
	@SuppressWarnings("unchecked")
	public static <T extends List<?>> T cast(Object obj) {
	    return (T) obj;
	}

//	public static List<Object> getArgs(/*ArrayList<Class> argClasses,*/ String json) {
////		List<Object> outArgs = new ArrayList<Object>(argClasses.size());
//		List args = g.fromJson(json, List.class);
//		try{
//			for(Class c : argClasses)
//				outArgs.add();
//		}catch( Exception e){
//			System.out.println(e.getClass());
//			e.printStackTrace();
//		}
//		return outArgs;
//	}
}
//	   for( Map<SlaveHandler, Object> s : slices) " (*"[1,2]";*)(*add loop through list*)
//	                                                	^ "       job.data += s.get(handler) + \"\"; "
//	                                                	^ "    job.data += \"]\""