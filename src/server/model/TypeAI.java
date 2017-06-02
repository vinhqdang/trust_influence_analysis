package server.model;

import java.io.Serializable;

public class TypeAI implements Serializable {

	private static final long serialVersionUID = 1L;
	private String modelAI;
	private Game game;

	public TypeAI(Game game) {
		this.modelAI = "Without AI";
		this.game = game;
	}

	public String getModelAI() {
		return modelAI;
	}

	public void setModelAI(String modelAI) {
		this.modelAI = modelAI;
	}

	public double playAI(double amountOpponent) {
		switch (getModelAI()) {
		case "With AI":
			return ((int) (Math.random() * 100) % amountOpponent);
		case "Without AI":
			return -1;
		default:
			break;
		}
		return -1;
	}

}
