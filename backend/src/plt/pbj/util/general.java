package plt.pbj.util;
import java.util.*;

import plt.pbj.*;
import plt.pbj.master.Master;
import plt.pbj.master.SlaveHandler;
import plt.pbj.master.Job;
import com.google.gson.Gson;

public class general {
	private static final Gson g = new Gson();
	
	public static void main( String[] args){
		LinkedList<Object> inNormal = new LinkedList<Object>();
//		LinkedList<Map<String, Object>> slices = slice(sh, inSliced);
		inNormal.push(new Integer(5));
		Object[] o = {
				(Object) new Integer(8),
				(Object) new Double(2.2)
		};
		inNormal.push(Arrays.asList(o));
		inNormal.push("hey");
		
		System.out.print(makeJsons(inNormal, inNormal));
	}
	
	public static void jamSliceSpread( 
			Master master, String className, 
			String globMethod, List< Object > globNormal, List< Object > globSliced,
			String method, List< Object > inNormal, List< Object > inSliced){
		
	}
	public static void sliceSpread( Master master, String className, String method, List< Object > inNormal, List< Object > inSliced){
		Set<SlaveHandler > sh = master.getSlaveHandlers();
		Map< String, LinkedList< Object > > slices = slice(sh, inSliced);
		Map<String, Job> jobs = new HashMap<String, Job>();
		for(SlaveHandler handler : master.getSlaveHandlers()) { 
	       Job job = new Job();
	       job.className = className;
	       job.method = method;
	       job.data = makeJsons( inNormal, slices.get(handler.getName()));
	       jobs.put( handler.getName(), job ); 
		} 
		master.spreadJobs(jobs);
	}
	
	
private static String makeJsons(List<Object> inNormal, LinkedList< Object> slices) {
		LinkedList<Object> l = new LinkedList<Object>();
		l.addAll(slices);

//		for( Object o : slices){
//			l.push(s.get( slaveName ));
//		}
		l.addAll(inNormal);
		return g.toJson(l);
	}
//       \"[" ^ (String.concat "," snd(acts)) ^ "\"" 

	private static Map< String, LinkedList< Object > > slice( 
			Set< SlaveHandler > sh, List<Object> objects) {
		Map< String, LinkedList< Object > > out = new HashMap< String, LinkedList< Object > >();
		for(SlaveHandler handler : sh)
			out.put(handler.getName(), new LinkedList<Object>());
		for( Object o : objects){
			if (o instanceof List){
				List<Object> l = (List<Object>) o;
				int sectionSize = l.size() / sh.size();
//				int heavySize = sectionSize + l.size() % sh.size();
				SlaveHandler last = null;
				int i = 0;
				for(SlaveHandler s : sh) {
					out.get(s.getName()).push((Object) l.subList(sectionSize * i, sectionSize * (i + 1)));
					if (i == sh.size() - 1){
						out.get(s.getName()).push((Object) l.subList(sectionSize * i, l.size() - 1));
					}
				}
			}
		}
		return out;
//		if (ob instanceof Map){
//			//not yet
//		}
//		return null;
	}
}
//	   for( Map<SlaveHandler, Object> s : slices) " (*"[1,2]";*)(*add loop through list*)
//	                                                	^ "       job.data += s.get(handler) + \"\"; "
//	                                                	^ "    job.data += \"]\""