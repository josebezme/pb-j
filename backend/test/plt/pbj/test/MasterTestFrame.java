package plt.pbj.test;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractListModel;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

import plt.pbj.master.Job;
import plt.pbj.master.Master;
import plt.pbj.master.Master.MasterObserver;
import plt.pbj.master.SlaveHandler;
import plt.pbj.slave.Slave;

@SuppressWarnings("serial")
public class MasterTestFrame extends JFrame implements ActionListener, MasterObserver {
	public static void main(String[] args) {
		new MasterTestFrame();
	
	}
	
	private static Border THIN_BORDER = new EmptyBorder(4, 4, 4, 4);

	private JList slaveList;

	private JButton runMaster;
	private JButton addSlave;
	private JButton runJobCount;

	private Master master;
	
	public MasterTestFrame() {
		super("PB&J Backend Master Tester");
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		Container contentPane = getContentPane();
		
		JPanel buttonPanel = new JPanel();
		buttonPanel.setSize(300, 150);
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.LINE_AXIS));
		buttonPanel.setBorder(THIN_BORDER);
		
		runMaster = new JButton("Run Master");
		runMaster.addActionListener(this);
		buttonPanel.add(runMaster);
		
		addSlave = new JButton("Add Slave");
		addSlave.addActionListener(this);
		buttonPanel.add(addSlave);
		
		runJobCount = new JButton("Run Job Count");
		runJobCount.addActionListener(this);
		buttonPanel.add(runJobCount);
		
		JPanel slavePanel = new JPanel();
		slavePanel.setLayout(new BorderLayout());
		slavePanel.setBorder(THIN_BORDER);
		
		slaveList = new JList();
		slaveList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		
		
		JScrollPane slaveScroll = new JScrollPane(slaveList);
		slaveScroll.setSize(300, 300);
		
		slavePanel.add(slaveScroll, BorderLayout.LINE_START);
		
		JPanel masterPanel = new JPanel();
		masterPanel.setLayout(new BorderLayout());
		masterPanel.add(buttonPanel, BorderLayout.PAGE_END);
		masterPanel.add(slavePanel, BorderLayout.CENTER);
		
		contentPane.add(masterPanel);
		
		refreshButtons();
		
		pack();
		setVisible(true);
	}
	
	private void refreshButtons() {
		if(master == null) {
			runMaster.setText("Run Master.");
			runJobCount.setEnabled(false);
			addSlave.setEnabled(false);
		} else {
			runMaster.setText("Stop Master.");
			runJobCount.setEnabled(true);
			addSlave.setEnabled(true);
		}
	}

	@Override
	public void actionPerformed(ActionEvent event) {
		if(event.getSource() == runMaster) {
			if(master == null) {
				master = new Master();
				master.addObserver(this);
				new Thread(master).start();
			} else {
				master.stop();
				master = null;
			}
			
			refreshButtons();
		} else if(event.getSource() == addSlave) {
			new Thread(new Slave()).start();
			
		} else if(event.getSource() == runJobCount) {
			System.out.println("Sending job to master.");
			
			Map<String, Job> jobs = new HashMap<String, Job>();
			
			for(SlaveHandler handler : master.getSlaveHandlers()) {
				Job job = new Job();
				job.className = "plt.pbj.test.sample.CountIntegers";
				job.method = "countIntegers";
				job.data = "[1,2]";
				jobs.put(handler.getName(), job);
			}
			
			master.spreadJobs(jobs);
		}
	}

	@Override
	public void notifySlaveChange() {
		System.out.println("Updating list model.");
		final List<SlaveHandler> slaveHandlers = 
				new ArrayList<SlaveHandler>(master.getSlaveHandlers());
		
		Collections.sort(slaveHandlers);
		
		System.out.println("Slav sockets: " + slaveHandlers);
		
		slaveList.setModel(new AbstractListModel() {

			@Override
			public Object getElementAt(int index) {
				return slaveHandlers.get(index).getName();
			}

			@Override
			public int getSize() {
				return slaveHandlers.size();
			}
			
		});
	}

	@Override
	public void notifySlaveDone() {
		System.out.println("A slave is done");
	}
}
