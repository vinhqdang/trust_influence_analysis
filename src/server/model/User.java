package server.model;

import java.io.Serializable;

public class User implements Serializable {

	private static final long serialVersionUID = 1L;
	private static final double INITIAL_TRUST_INDEX = 1.0;

	/**
	 * @param money
	 *            : the total amount of money that a user has
	 * @param moneyReceived
	 *            : the money that a user receives, tripled
	 * @param singleMoneyReceived
	 *            : the money that a user receives, not tripled
	 */
	private String mailAdress;
	private String password;
	private int turn;
	private double money;
	private double moneyReceived;
	private double singleMoneyReceived;
	private double trustIndex = INITIAL_TRUST_INDEX;
	private double trustIndexOpponent = INITIAL_TRUST_INDEX;

	public User(String mail, String password, double initialMoney,
			double trustIndex, int turn) {
		this.mailAdress = mail;
		this.password = password;
		this.money = initialMoney;
		this.trustIndex = trustIndex;
		this.turn = turn;
	}

	public void spend(double amount) {
		if (amount > this.money)
			throw new IllegalArgumentException("Not enough money!");
		if (amount < 0)
			throw new IllegalArgumentException(
					"Negative numbers are forbidden!");
		this.money -= amount;
	}

	public void earn(double earning) {
		if (earning < 0)
			throw new IllegalArgumentException("Inconceivable!");
		this.money += earning;
	}

	public double firstRound(User a) {
		double newTrustIndex = a.getTrustIndex();
		if (this.getSingleMoneyReceived() >= 0
				&& this.getSingleMoneyReceived() < a.getSingleMoneyReceived()) {
			newTrustIndex = this.getSingleMoneyReceived()
					/ a.getSingleMoneyReceived() + newTrustIndex - 1;
			if (newTrustIndex < 0) {
				newTrustIndex = 0;
			}
		}
		if (this.getSingleMoneyReceived() > a.getSingleMoneyReceived()
				&& this.getSingleMoneyReceived() <= 3 * a
						.getSingleMoneyReceived()) {
			newTrustIndex = a.getTrustIndex() + this.getSingleMoneyReceived()
					/ (3 * a.getSingleMoneyReceived());
			if (newTrustIndex > 1) {
				newTrustIndex = 1;
			}
		}

		return newTrustIndex;
	}

	public void newTrustCalculator(User a, double c, int period) {
		double previousTrust = -1;
		if (period == 0) {
			a.setTrustIndex(firstRound(a));
		} else {
			previousTrust = a.getTrustIndex();
			double currentTrust = firstRound(a);
			double threshold = 0.25;
			double x = 0;
			x = c * Math.abs(previousTrust - currentTrust) + (1 - c) * x;
			double alpha = threshold + c
					* (Math.abs(currentTrust - previousTrust) / (1 + x));
			double newTrustIndex = alpha * currentTrust + (1 - alpha)
					* previousTrust;
			a.setTrustIndex(newTrustIndex);
		}
	}

	public int getTurn() {
		return turn;
	}

	public double getTrustIndex() {
		return trustIndex;
	}

	public void setTrustIndex(double trustIndex) {
		this.trustIndex = trustIndex;
	}
	
	public double getTrustIndexOpponent() {
		return this.trustIndexOpponent;
		//return this.opponent.getTrustIndex();
	}
	
	public void setTrustIndexOpponent(double trustIndex) {
		this.trustIndexOpponent=trustIndex;
		//this.opponent.setTrustIndex(trustIndex);
	}

	public double getMoney() {
		return this.money;
	}

	public void setMoney(double money) {
		this.money = money;
	}

	public double getMoneyReceived() {
		return this.moneyReceived;
	}

	public void setMoneyReceived(double money) {
		this.moneyReceived = money;
	}

	public double getSingleMoneyReceived() {
		return singleMoneyReceived;
	}

	public void setSingleMoneyReceived(double singleMoneyReceived) {
		this.singleMoneyReceived = singleMoneyReceived;
	}

	public String getId() {
		return mailAdress;
	}

	public String getPassword() {
		return password;
	}

	public String userToString() {
		return "USER: \n - Mail Address: " + this.mailAdress
				+ "\n - Initial Money: " + this.money + "\n";
	}
}