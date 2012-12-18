package plt.pbj.test.sample;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;

import plt.pbj.master2.Master;
import plt.pbj.master2.SlaveHandler;
import plt.pbj.util.PBJOp;

public class TestJamSpread {

	private static final Gson gson = new Gson();
	
	public static Integer doNext( Map<String, Object> returns, LinkedList<Object> args){
	String s = (String) args.pop();
	
	System.out.println("Did something else");
	return returns.size();
	}



	public static Integer doFirst( LinkedList<Object> args ){
//	LinkedList args = gson.fromJson(data, LinkedList.class);
	Long l = ((Double) args.pop()).longValue(); 
	String s = (String) args.pop();
	
	System.out.println(s);
	return new Integer(1);

	}



	public static void master(Master master, List<SlaveHandler> slaves, List<Object> args){
	Object[] normActuals = { (Object)new Long(10), (Object)"apple"};
	Collection[] slicedActuals = { (Collection)new ArrayList<Object> (Arrays.asList(new Long
			(5), new Long(10), new Long(10)))};
	Object[] jnormActuals = { (Object)"lemon"};
	Integer i = (Integer) PBJOp.jamSliceSpread( master, Thread.currentThread().getStackTrace()[1].getClassName(), 
			"doNext", Arrays.asList(jnormActuals), "doFirst", Arrays.asList(normActuals), 
			Arrays.asList((Collection<Object>[]) slicedActuals));
	System.out.print(i);
	}

}
