package server.model;

public class TrustUser {

	private double contribution;
	private int turn;
	private double trustIndex;

	/**
	 * Build a User using parameters to measure trust
	 * 
	 * @param contribution
	 *            The contribution of the player
	 * @param trustIndex
	 *            The value between 0 and 1 which reflects the value of trust
	 *            other users have regarding this specific user
	 * @param turn
	 *            Indicated who starts. 0 if the player starts first, 1 if it
	 *            starts second
	 */
	public TrustUser(double contribution, int turn) {
		this.contribution = contribution;
		this.turn = turn;
	}

	public void initialize() {
		this.trustIndex = 1;
	}

	public double firstRound(TrustUser a) {
		double newTrustIndex = a.getTrustIndex();
		if (a.getContribution() >= 0
				&& a.getContribution() < this.getContribution()) {
			newTrustIndex = a.getContribution() / this.getContribution()
					+ newTrustIndex - 1;
			System.out.println(">>>>>>>>> firstRound: not trusted");
			if (newTrustIndex < 0) {
				newTrustIndex = 0;
			}
		}
		if (a.getContribution() > this.getContribution()
				&& a.getContribution() <= 3 * this.getContribution()) {
			newTrustIndex = a.getTrustIndex() + a.getContribution()
					/ (3 * this.getContribution());
			System.out.println(">>>>>>>>> firstRound: trusted");
			if (newTrustIndex > 1) {
				newTrustIndex = 1;
			}
		}
		return newTrustIndex;
	}

	public double otherRound(TrustUser a) {
		double oldTrustIndex = a.getTrustIndex();
		double newTrustIndex = firstRound(a);
		System.out.println("old trust index: " + oldTrustIndex);
		System.out.println("newTrustIndex: " + newTrustIndex);
		if (a.getTurn() == 0) {
			newTrustIndex = oldTrustIndex;
		} else {
			if (oldTrustIndex > newTrustIndex) {
				newTrustIndex = (oldTrustIndex - newTrustIndex) / 2;
				System.out.println(">>>>>>>>> secondRound: not trusted");
			}
			if (oldTrustIndex < newTrustIndex) {
				newTrustIndex = (newTrustIndex + oldTrustIndex) / 2;
				System.out.println(">>>>>>>>> secondRound: trusted");
			}
			if (oldTrustIndex == newTrustIndex) {
				newTrustIndex = oldTrustIndex;
				System.out.println(">>>>>>>>> secondRound: suspicion");
			}
		}
		return newTrustIndex;
	}

	public void newTrustCalculator(TrustUser a, double c, int period) {
		// we start with c = 1/2
		double previousTrust = -1;
		double newTrustIndex = -1;

		if (a.getTurn() == 0) {
			newTrustIndex = a.getTrustIndex();
		}

		else {
			if (period == 1) {
				a.initialize();
				newTrustIndex = firstRound(a);
			}

			else {
				previousTrust = a.getTrustIndex();

				double currentTrust = firstRound(a);
				double threshold = 0.25;
				double x = 0;

				x = c * Math.abs(previousTrust - currentTrust) + (1 - c) * x;
				double alpha = threshold + c
						* (Math.abs(currentTrust - previousTrust) / (1 + x));
				newTrustIndex = alpha * currentTrust + (1 - alpha)
						* previousTrust;
			}
		}

		a.setTrustIndex(newTrustIndex);
	}

	public double getContribution() {
		return this.contribution;
	}

	public void setContribution(double contribution) {
		this.contribution = contribution;
	}

	public int getTurn() {
		return turn;
	}

	public double getTrustIndex() {
		return this.trustIndex;
	}

	public void setTrustIndex(double trustIndex) {
		this.trustIndex = trustIndex;
	}

}
