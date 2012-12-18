package plt.pbj.test.sample;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import plt.pbj.master2.Master;
import plt.pbj.util.PBJOp;

public class TestSpread {

	public static String doSomething() {
		System.out.println("Did something");
		return "hey";

	}

	public static void master(/*Map<Object, Object> slaves,*/ Master master, List<Object> args) {
		String className = Thread.currentThread().getStackTrace()[1].getClassName();
		Object[] normActuals = { (Object) new Long(10), (Object) "apple" };
		Collection[] slicedActuals = { (Collection<Object>) new ArrayList<Object>(
				Arrays.asList(new Long(5), new Long(10), new Long(10))) };
//		Collection<Object>[] slicedReal = (Collection<Object>[]) slicedActuals;
		PBJOp.sliceSpread(master, className, "doSomething",
				Arrays.asList(normActuals), Arrays.asList((Collection<Object>[]) slicedActuals));

	}

}
