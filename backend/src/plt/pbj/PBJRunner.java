package plt.pbj;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import plt.pbj.util.PBJOp;
public class PBJRunner {
	
	
	public static void master(Map<Object, Object> slaves, List<Object> args){
		
		List<Object> list = new ArrayList<Object>();
		list.add("args");
		
		String result = (String) PBJOp.spread("test", new Object[]{new Spreadable(list)});
		System.out.println("Spread Result: " + result);
		
		list.clear();
		list.add(new Long(1));
		list.add(new Long(2));
		list.add(new Long(3));
		list.add(new Long(4));
		list.add(new Long(5));
		list.add(new Long(6));
		list.add(new Long(7));
		
		List<Object> objs = PBJOp.jam("add", new Object[]{ new Spreadable(list)});
		Long result_l = add(objs);
		
//		try {
//			Thread.sleep(3000);
//		} catch (InterruptedException e) {
//			e.printStackTrace();
//		}		
		
		Long l = add(list);
		System.out.println("Jam Result: " + result_l);
		System.out.println("Should be:  " + l);

	}
	
	public static Long add(List<Object> arg1) {
		Long l = 0L;
		
		for(Object o : arg1) {
			l += (Long) o;
		}
		
		return l;
	}
	
	public static String test(List<Object> arg1) {
		
		int r = new Random().nextInt(3);
		
		try {
			Thread.sleep(r * 1000);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		return "It worked";
	}

}