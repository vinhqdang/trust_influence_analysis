package server.model;

public class TrustUser2 {
	
	private double moneyReceived;
	private int turn;
	private double trustIndex;
	
	public TrustUser2(double moneyReceived, int turn) {
		this.moneyReceived = moneyReceived;
		this.turn = turn;
	}
	
	public void initialize() {
		this.trustIndex = 1;
	}

	public double firstRound(TrustUser2 a) {
		double newTrustIndex = a.getTrustIndex();
		if (this.getMoneyReceived() >= 0
				&& this.getMoneyReceived() < a.getMoneyReceived()) {
			newTrustIndex = this.getMoneyReceived() / a.getMoneyReceived()
					+ newTrustIndex - 1;
			System.out.println(">>>>>>>>> firstRound: not trusted");
			if (newTrustIndex < 0) {
				newTrustIndex = 0;
			}
		}
		if (this.getMoneyReceived() > a.getMoneyReceived()
				&& this.getMoneyReceived() <= 3 * a.getMoneyReceived()) {
			newTrustIndex = a.getTrustIndex() + this.getMoneyReceived()
					/ (3 * a.getMoneyReceived());
			System.out.println(">>>>>>>>> firstRound: trusted");
			if (newTrustIndex > 1) {
				newTrustIndex = 1;
			}
		}
		return newTrustIndex;
	}
	
	public void newTrustCalculator(TrustUser2 a, double c, int period) {
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
	
	public int getTurn() {
		return turn;
	}

	public double getTrustIndex() {
		return this.trustIndex;
	}

	public void setTrustIndex(double trustIndex) {
		this.trustIndex = trustIndex;
	}

	public double getMoneyReceived() {
		return moneyReceived;
	}

	public void setMoneyReceived(double moneyReceived) {
		this.moneyReceived = moneyReceived;
	}

}
