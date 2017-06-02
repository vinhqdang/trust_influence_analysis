package client.application;

import java.io.IOException;
import java.rmi.RemoteException;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;

import common.Histogram;
import common.IUserSession;

public class PlayClientController {

	@FXML
	private AnchorPane play;
	@FXML
	private Label moneyPlayer;
	@FXML
	private Label trustIndexLabel;
	@FXML
	private Label opponentTrustIndexLabel;
	@FXML
	private Label infoPlayer;
	@FXML
	private Label errorLabel;
	@FXML
	private Label maximumAmountLabel;
	@FXML
	private Label namePlayer;
	@FXML
	private TextField amountMoneyPlayer;
	@FXML
	private Button validatePlayer;
	@FXML
	private HistogramView histogramView;

	private IUserSession session;
	private double timeStart = 0;
	final private String nameOfPlayer;

	public PlayClientController(IUserSession session, Histogram histogram,
			String nameOfPlayer) {
		this.session = session;
		this.nameOfPlayer = nameOfPlayer;
	}

	/**
	 * Initializes the controller class.
	 * 
	 * @throws RemoteException
	 */
	public void initialize() throws RemoteException {
		play.setCache(false);
		this.errorLabel.setVisible(false);
		moneyPlayer.setText("" + session.getMoneyUser());
		trustIndexLabel.setText("" + session.getTrustIndex());
		opponentTrustIndexLabel.setText("" + session.getTrustIndexOpponent());
		maximumAmountLabel.setText("" + session.getMoneyUser());
		namePlayer
				.setText("Hello Player " + parseMailAdress(this.nameOfPlayer));

		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				validatePlayer.requestFocus();
			}
		});
	}

	public String parseMailAdress(String mail) {
		String parseMail = "";
		if (mail.indexOf("@") == -1)
			return mail;
		else {
			for (int i = 0; i < mail.indexOf("@"); i++) {
				parseMail = parseMail + "" + mail.charAt(i);
			}
			return parseMail + "\n";
		}
	}

	/**
	 * Called when the Validate button is fired.
	 * 
	 * @param event
	 *            the action event.
	 * @throws NumberFormatException
	 * @throws IOException
	 * @throws ClassNotFoundException
	 */
	public void validateIssueFired(ActionEvent event)
			throws NumberFormatException, ClassNotFoundException, IOException {
		if (this.amountMoneyPlayer.getText().matches("[0-9]+")) {
			double amount = Double.valueOf(amountMoneyPlayer.getText());
			if (amount <= this.session.getMoneyUser()) {
				if (amount <= Double.valueOf(maximumAmountLabel.getText())) {
					errorLabel.setVisible(false);
					validatePlayer.setDisable(true);
					amountMoneyPlayer.setDisable(true);
					infoPlayer.setText("Waiting for your opponent...");
					double time = (System.currentTimeMillis() - timeStart) / 1000;
					session.play(amount, time);
					moneyPlayer.setText("" + session.getMoneyUser());
					trustIndexLabel.setText("" + session.getTrustIndex());
					opponentTrustIndexLabel.setText(""
							+ session.getTrustIndexOpponent());
					maximumAmountLabel.setVisible(false);
				} else {
					errorLabel
							.setText("You can't play more than the triple of the sum you received!");
					errorLabel.setVisible(true);
				}
			} else {
				errorLabel.setText("You don't have enough money.");
				errorLabel.setVisible(true);
			}
		} else {
			errorLabel.setText("Incorrect value!");
			errorLabel.setVisible(true);
		}
	}

	public void myTurn(double amount) throws RemoteException {
		infoPlayer.setText("Your opponent plays " + amount
				+ ", now it's your turn.");
		validatePlayer.setDisable(false);
		amountMoneyPlayer.clear();
		amountMoneyPlayer.setDisable(false);
		amountMoneyPlayer.requestFocus();
		timeStart = System.currentTimeMillis();
		moneyPlayer.setText("" + session.getMoneyUser());
		trustIndexLabel.setText("" + session.getTrustIndex());
		opponentTrustIndexLabel.setText("" + session.getTrustIndexOpponent());
		maximumAmountLabel.setText("" + session.getMoneyReceived());
		maximumAmountLabel.setVisible(true);
	}

	public void firstTurn(boolean myTurn) {
		if (myTurn) {
			infoPlayer.setText("Let's start. Enter a number.");
		} else {
			infoPlayer.setText("Your opponent starts.");
			maximumAmountLabel.setVisible(false);
		}
		validatePlayer.setDisable(!myTurn);
		amountMoneyPlayer.setDisable(!myTurn);
		timeStart = System.currentTimeMillis();
	}

	public void gameFinished(final boolean firstPlayerZero) {
		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				try {
					moneyPlayer.setText("" + session.getMoneyUser());
					trustIndexLabel.setText("" + session.getTrustIndex());
					opponentTrustIndexLabel.setText(""
							+ session.getTrustIndexOpponent());
					maximumAmountLabel.setText("" + session.getMoneyReceived());
				} catch (RemoteException e) {
					e.printStackTrace();
				}
				if (firstPlayerZero)
					infoPlayer.setText("Your opponent plays 0, game over.");
				else
					infoPlayer.setText("The game is finished.");

				validatePlayer.setDisable(true);
			}
		});
	}
}
