package plt.pbj;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import plt.pbj.master.Master;
import plt.pbj.master.SlaveHandler;
import plt.pbj.slave.Slave;
import plt.pbj.slave.SlaveAddress;

public class PBJ {
	public static Gson gson = new GsonBuilder()
			.registerTypeAdapter(Double.class, new JsonSerializer<Double>() {
				@Override
				public JsonElement serialize(Double d, Type arg1,
						JsonSerializationContext arg2) {
					return new JsonPrimitive("d_" + d);
				}
			})
			.registerTypeAdapter(Long.class, new JsonSerializer<Long>() {
				@Override
				public JsonElement serialize(Long l, Type arg1,
						JsonSerializationContext arg2) {
					return new JsonPrimitive("l_" + l);
				}
			}).create();
	
	
	public static void main(String[] args) {
		if(args.length < 1) {
			printUsage();
			System.exit(1);
		}
		
		if(args[0].equals("-slave")) {
			
			int portNumber = Master.DEFAULT_PORT;
			if(args.length > 1) {
				try {
					portNumber = Integer.parseInt(args[1]);
				} catch(Exception e) {
					printUsage();
					System.err.println("Invalid port number: " + args[1]);
					System.exit(1);
				}
				
				
			}
			
			runSlave(portNumber);
		} else {
			String[] ipAddresses = args[0].split(";");
			
			if(ipAddresses.length > 0) {
				
				List<SlaveAddress> sAddresses = new ArrayList<SlaveAddress>();
				SlaveAddress sAddress;
				for(String address : ipAddresses) {
					try {
						if(address.contains(":")) {
							String parts[] = address.split(":");
							
							sAddress = new SlaveAddress(
										parts[0],
										Integer.parseInt(parts[1]));
							
						} else {
							sAddress = new SlaveAddress(
											address, 
											Master.DEFAULT_PORT);
						}
					
						sAddresses.add(sAddress);
					
					} catch (Exception e) {
						printUsage();
						System.err.println("Exception in address: " + address);
						System.exit(1);
					}
				}
				Master master = new Master(sAddresses);
				
				synchronized(master) {
					new Thread(master).start();
					
					
					try {
						// Wait until slaves have initialized.
						master.wait();
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
					
					// Done waiting... run that shit.
					
					Object[] masterArgs = Arrays.copyOfRange(args, 1, args.length);
					System.out.println("args: " + Arrays.toString(args));
					System.out.println("master args: " + Arrays.toString(masterArgs));
					
					Map<Object, Object> slaves = new HashMap<Object, Object>();
					
					for(SlaveHandler handler : master.getSlaveHandlers()) {
						slaves.put(handler.getName(), "more info later");
					}
					
					PBJRunner.master(slaves, Arrays.asList(masterArgs));
					
					master.close();
					
				}
				
				
				
				
				
				
			} else {
				printUsage();
				System.err.println("No slave addresses provided.");
				System.exit(1);
			}
		}
	}

	private static void printUsage() {
		System.err.println("Usage: ");
		System.err.println("\tFor Slave: {PBJ} -slave [portNumber]");
		System.err.println("\tFor Master:{PBJ} ip[:PORT];ip2[:PORT];...");
		
	}

	private static void runSlave(int portNumber) {
		new Thread(new Slave(portNumber)).start();
	}
}
