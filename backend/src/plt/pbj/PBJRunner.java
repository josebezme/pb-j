package plt.pbj;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import plt.pbj.util.PBJOp;
public class PBJRunner {
	
	
	public static void master(Map<Object, Object> slaves, List<Object> args){
		
		List<Object> list = new ArrayList<Object>();
		list.add(new Long(1));
		list.add(new Long(2));
		
		Object obj = PBJOp.spread("test", new Object[]{ new Spreadable(list)});
		
		System.out.println("Jam Result: " + obj);

	}
	
	public static Long add(List<Object> arg1) {
		Long l = 0L;
		
		for(Object o : arg1) {
			l += (Long) o;
		}
		
		return l;
	}
	
	public static String test(List<Object> arg1) {
		
//		int r = new Random().nextInt(3);
		Long l = (Long) arg1.get(0);
		
		if(l == 1) {
			try {
				Thread.sleep(3000);
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			
			return "1";
		}
		
		
		
		return null;
	}

}