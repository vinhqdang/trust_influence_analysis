package client.application;

import java.io.IOException;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import javafx.application.Platform;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;
import client.utilities.RunRMI;

import common.IGameEventsListener;
import common.IServerRMI;
import common.IUserSession;

public class MainFrameClientController implements IGameEventsListener {

	private IGameEventsListener exportListener;
	private IUserSession session;
	private PlayClientController playController;
	private IServerRMI remoteServer;
	private boolean showHistogram;
	private boolean showButton;

	@FXML
	private AnchorPane mainFrame;
	@FXML
	private Button playButton;
	@FXML
	private Button showStatisticsButton;
	@FXML
	private Button connectButton;
	@FXML
	private Button disconnectionButton;
	@FXML
	private TextField emailAddressField;
	@FXML
	private PasswordField passwordField;
	@FXML
	private PasswordField confirmPasswordField;
	@FXML
	private Label statusLabel;

	/**
	 * Initializes the controller class.
	 * 
	 * @throws RemoteException
	 */
	public void initialize() throws RemoteException {
		playButton.setVisible(false);
		showStatisticsButton.setVisible(false);
		mainFrame.setCache(true);
		remoteServer = new RunRMI().getStub();
		exportListener = (IGameEventsListener) UnicastRemoteObject
				.exportObject(this, 0);
	}

	public void connectionIssueFired(ActionEvent event)
			throws NumberFormatException, IOException, ClassNotFoundException {
		if (emailAddressField
				.getText()
				.matches(
						"^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$")) {
			if (!passwordField.getText().isEmpty()) {
				if (passwordField.getText().equals(confirmPasswordField.getText())) {
					System.out.println();
					session = remoteServer.login(emailAddressField.getText(),
							passwordField.getText(), exportListener);
					if (session != null) {
						setButtonGame(session.getGameChoice());
						statusLabel.setText("Click to play.");
						playButton.setVisible(true);
						if (session.getGameChoice().contains("with histo")) {
							showStatisticsButton.setVisible(true);
						}
					} else {
						statusLabel.setText("Incorrect password!");
					}
				} else {
					statusLabel.setText("Passwords do not match!");
				}
			} else {
				statusLabel.setText("You must enter a password!");
			}
		} else {
			statusLabel.setText("Email address not valid!");
		}
	}

	public void disconnectionIssueFired(ActionEvent event)
			throws NumberFormatException, IOException, ClassNotFoundException {
		if (session != null) {
			session.logout();
			emailAddressField.clear();
			passwordField.clear();
			confirmPasswordField.clear();
			playButton.setVisible(false);
			showStatisticsButton.setVisible(false);
			statusLabel.setText("Please connect!");
		} else {
			statusLabel.setText("Please connect!");
		}
	}

	public void playIssueFired(ActionEvent event) throws NumberFormatException,
			IOException, ClassNotFoundException {
		if (session == null) {
			statusLabel.setText("You are not logged");
		} else {
			joinRoom(this.showHistogram);
			playButton.setVisible(showButton);
		}
	}

	public void showStatisticsButtonPressed(ActionEvent event)
			throws RemoteException {
		if (session != null) {
			Stage stage = new Stage();
			HistogramView view = new HistogramView();
			view.setHistogram(remoteServer.getHistogram(), session.getUsers());
			Scene scene = new Scene(view);
			stage.setScene(scene);
			stage.showAndWait();
		} else {
			statusLabel.setText("You are not logged !");
		}
	}

	private void joinRoom(boolean showHistogram) throws ClassNotFoundException,
			IOException {
		this.showHistogram = showHistogram;
		session.joinRoom();
		statusLabel.setText("Waiting for an opponent...");
	}

	private void playScreen() throws IOException {
		Stage secondStage = new Stage();
		FXMLLoader loader = new FXMLLoader(getClass().getResource("Play.fxml"));
		playController = new PlayClientController(session,
				showHistogram ? remoteServer.getHistogram() : null,
				emailAddressField.getText());
		loader.setController(playController);

		AnchorPane play = (AnchorPane) loader.load();
		Scene scenePlay = new Scene(play, 800, 500);
		scenePlay.getStylesheets().add(
				getClass().getResource("application.css").toExternalForm());
		secondStage.setScene(scenePlay);
		secondStage.show();
		statusLabel.setText("Enjoy your game!");
	}

	@Override
	public void myTurn(final double amount) {
		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				try {
					playController.myTurn(amount);
				} catch (RemoteException e) {
					e.printStackTrace();
				}
			}
		});
	}

	@Override
	public void firstTurn(final boolean myTurn) {
		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				try {
					playScreen();
				} catch (IOException e) {
					e.printStackTrace();
				}
				playController.firstTurn(myTurn);
			}
		});
	}

	@Override
	public void gameFinished(boolean firstPlayerZero) {
		if (playController != null) {
			playController.gameFinished(firstPlayerZero);
		}
	}

	@Override
	public void setButtonGame(String gameChoice) throws RemoteException {
		switch (gameChoice) {
		case "Play with histo and with repetition":
			this.showHistogram = true;
			this.showButton = true;
			break;
		case "Play with histo and without repetition":
			this.showHistogram = true;
			this.showButton = false;
			break;
		case "Play without histo and with repetition":
			this.showHistogram = false;
			this.showButton = true;
			break;
		case "Play without histo and without repetition":
			this.showHistogram = false;
			this.showButton = false;
			break;
		default:
			break;
		}
	}
}
