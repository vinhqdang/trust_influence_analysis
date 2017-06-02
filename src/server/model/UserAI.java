package server.model;

import java.io.IOException;
import java.io.Serializable;
import java.rmi.RemoteException;

import javafx.application.Platform;
import server.application.UserSession;
import common.IGameEventsListener;

public class UserAI extends User {

	private static final long serialVersionUID = 1L;
	
	private TypeAI typeAI;
	private AIListener aiListener;
	private UserSession sessionAI;

	public UserAI(String id, double initialMoney, TypeAI typeAI) {
		super(id, null, initialMoney, -1, 0);
		this.typeAI = typeAI;
		this.aiListener = new AIListener();
	}

	public TypeAI getTypeAI() {
		return typeAI;
	}

	public void setTypeAI(TypeAI typeAI) {
		this.typeAI = typeAI;
	}

	public AIListener getAiListener() {
		return aiListener;
	}

	public void setSessionAI(UserSession sessionAI) {
		this.sessionAI = sessionAI;
	}
	
	public double getMaxAmount() {
		return this.sessionAI.getGame().getAmountPerUser();
	}

	private class AIListener implements IGameEventsListener, Serializable {

		private static final long serialVersionUID = 1L;

		@Override
		public void myTurn(final double amount) {
			Platform.runLater(new Runnable() {
				@Override
				public void run() {
					try {
						sessionAI.play(getTypeAI().playAI(amount), 0);
					} catch (ClassNotFoundException | IOException e) {
						e.printStackTrace();
					}
				}
			});
		}

		@Override
		public void firstTurn(boolean yourTurn) throws RemoteException {
			if (yourTurn)
				myTurn(getMaxAmount());
		}

		@Override
		public void gameFinished(boolean firstPlayerZero)
				throws RemoteException {

		}

		@Override
		public void setButtonGame(String gameChoice) throws RemoteException {

		}

	}
}
