package common;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.TreeSet;

public class Histogram implements Serializable, Comparator<DataSample> {

	private static final long serialVersionUID = 1L;

	private TreeSet<DataSample> dataSamples;

	public Histogram() {
		dataSamples = new TreeSet<DataSample>(this);
	}

	@Override
	public int compare(DataSample o1, DataSample o2) {
		int cmp = -Double.compare(o1.getGivenAmount(), o2.getGivenAmount());

		if (cmp == 0) {
			return -Double.compare(o1.getReceivedAmount(),
					o2.getReceivedAmount());
		} else {
			return cmp;
		}
	}

	public TreeSet<DataSample> getDataSamples() {
		return dataSamples;
	}

	public ArrayList<DataGroup> getDataGroups() {
		ArrayList<DataGroup> groups = new ArrayList<DataGroup>();

		DataGroup group = null;

		for (DataSample sample : dataSamples) {
			if (group == null) {
				group = new DataGroup(sample.getGivenAmount());
				groups.add(group);
			}

			if (group.key != sample.getGivenAmount())
				group = new DataGroup(sample.getGivenAmount());

			group.data.add(sample);
		}

		if (group != null)
			groups.add(group);

		return groups;
	}
}
