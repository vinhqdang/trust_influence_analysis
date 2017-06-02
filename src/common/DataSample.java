package common;

import java.io.Serializable;

import server.model.User;

public class DataSample implements Serializable {
	private static final long serialVersionUID = 1L;

	private double givenAmount;
	private double receivedAmount;
	private double timeToPlay;
	private int period;
	private User me;
	private User opponent;

	public DataSample() {
		this.givenAmount = 0;
		this.receivedAmount = 0;
		this.timeToPlay = 0;
		this.period = 0;
		this.me = null;
		this.opponent = null;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		long temp;
		temp = Double.doubleToLongBits(givenAmount);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(receivedAmount);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		temp = Double.doubleToLongBits(timeToPlay);
		result = prime * result + (int) (temp ^ (temp >>> 32));
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		DataSample other = (DataSample) obj;
		if (Double.doubleToLongBits(givenAmount) != Double
				.doubleToLongBits(other.givenAmount))
			return false;
		if (Double.doubleToLongBits(receivedAmount) != Double
				.doubleToLongBits(other.receivedAmount))
			return false;
		if (Double.doubleToLongBits(timeToPlay) != Double
				.doubleToLongBits(other.timeToPlay))
			return false;
		return true;
	}

	public void addGivenAmount(double givenAmount) {
		this.givenAmount += givenAmount;
	}

	public void addReceivedAmount(double receivedAmount) {
		this.receivedAmount += receivedAmount;
	}

	public User getMe() {
		return this.me;
	}

	public void setMe(User me) {
		this.me = me;
	}

	public User getOpponent() {
		return this.opponent;
	}

	public void setOpponent(User opponent) {
		this.opponent = opponent;
	}

	public double getGivenAmount() {
		return givenAmount;
	}

	public void setGivenAmount(double givenAmount) {
		this.givenAmount = givenAmount;
	}

	public double getReceivedAmount() {
		return receivedAmount;
	}

	public void setReceivedAmount(double receivedAmount) {
		this.receivedAmount = receivedAmount;
	}

	public double getTimeToPlay() {
		return timeToPlay;
	}

	public void setTimeToPlay(double timeToPlay) {
		this.timeToPlay = timeToPlay;
	}

	public int getPeriod() {
		return period;
	}

	public void setPeriod(int period) {
		this.period = period;
	}
}
