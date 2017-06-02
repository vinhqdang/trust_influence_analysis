package server.application;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.ArrayList;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import server.model.Game;
import server.model.TypeAI;
import server.utilities.ServerRMI;
import common.FileSaver;

public class FrameServerController {

	private int numberOfRepetitionsPerRoom;
	private int numberOfUsersPerRoom;
	private double amountPerUser;
	private ServerRMI server;
	private String gameChoice;
	private String aiChoice;
	private TypeAI returnAI;
	private FileSaver fileSaver;
	private int numberOfAIPerRoom;

	@FXML
	AnchorPane serverParameters;
	@FXML
	TextField number_of_repetitions_per_room;
	@FXML
	TextField amount_per_user;
	@FXML
	Button ok;
	@FXML
	Label error_label;
	@FXML
	Label nbAILabel;
	@FXML
	ComboBox<String> game;
	@FXML
	ComboBox<String> typeAI;

	public FrameServerController(ServerRMI server) {
		this.server = server;
		this.returnAI = new TypeAI(server.getGame());
		this.fileSaver = new FileSaver();
		if (!this.fileSaver.getSettingsServer().exists()) {
			this.fileSaver.generateCsvServer();
		}
		try {
			server.setGame(new Game(2, 2, 10,
					"Play without histo and without repetition", 0,
					this.returnAI));
			server.setGame(server.getGame().load());
		} catch (IOException e1) {}
	}

	/**
	 * Initializes the frame server controller class.
	 */
	public void initialize() {
		serverParameters.setCache(false);
		this.error_label.setVisible(false);

		Platform.runLater(new Runnable() {
			@Override
			public void run() {
				ok.requestFocus();
			}
		});

		gameChoice = "";
		aiChoice = "";
		numberOfAIPerRoom = 1;

		ArrayList<String> gameList = new ArrayList<String>();
		gameList.add("Play with histo and with repetition");
		gameList.add("Play with histo and without repetition");
		gameList.add("Play without histo and with repetition");
		gameList.add("Play without histo and without repetition");
		ObservableList<String> observableGameList = FXCollections
				.observableList(gameList);
		observableGameList.addListener(new ListChangeListener() {

			@Override
			public void onChanged(ListChangeListener.Change change) {
			}
		});
		game.setItems(observableGameList);

		ArrayList<String> iaList = new ArrayList<String>();
		iaList.add("Without AI");
		iaList.add("With AI");
		ObservableList<String> observableIaList = FXCollections
				.observableList(iaList);
		observableIaList.addListener(new ListChangeListener() {

			@Override
			public void onChanged(ListChangeListener.Change change) {
			}
		});
		typeAI.setItems(observableIaList);
	}

	/**
	 * Called when the admin pressed the OK Button
	 * 
	 * @param evt
	 *            The ActionEvent called when the button is pressed
	 * @throws RemoteException
	 */
	public void OKIssueFired(ActionEvent evt) throws RemoteException {
		this.error_label.setVisible(false);
		if (this.gameChoice == "Play with histo and with repetition"
				|| this.gameChoice == "Play with histo and without repetition"
				|| this.gameChoice == "Play without histo and with repetition"
				|| this.gameChoice == "Play without histo and without repetition") {

			if (this.number_of_repetitions_per_room.getText() != null
					&& this.amount_per_user.getText() != null
					&& !this.number_of_repetitions_per_room.getText().isEmpty()
					&& !this.amount_per_user.getText().isEmpty()) {

				if (this.number_of_repetitions_per_room.getText().matches(
						"[0-9]+")
						&& this.amount_per_user.getText().matches("[0-9]+")) {

					this.numberOfRepetitionsPerRoom = Integer
							.parseInt(number_of_repetitions_per_room.getText());
					this.numberOfUsersPerRoom = 2;
					this.amountPerUser = Double.parseDouble(amount_per_user
							.getText());

					if (this.numberOfRepetitionsPerRoom <= 20) {

						if (this.amountPerUser <= 100) {

							if (this.aiChoice == "Without AI"
									|| this.aiChoice == "With AI") {

								this.aiChoice = typeAI.getValue();
								returnAI.setModelAI(this.aiChoice);
								Game current = new Game(
										this.numberOfUsersPerRoom,
										this.numberOfRepetitionsPerRoom
												* this.numberOfUsersPerRoom,
										this.amountPerUser, this.gameChoice,
										this.numberOfAIPerRoom, returnAI);
								
								//without cumulative endowment
								//current.setEndowment(this.amountPerUser);
								
								this.server.setGame(current);
								this.fileSaver.setGame(current);
								this.fileSaver.writeCsvServer();

							} else {
								ok.requestFocus();
								this.error_label
										.setText("Please specify the AI type!");
								this.error_label.setVisible(true);
							}
						}

						else {
							ok.requestFocus();
							this.error_label
									.setText("The initial endowment cannot exceed $100");
							this.error_label.setVisible(true);
						}
					}

					else {
						ok.requestFocus();
						this.error_label
								.setText("Only 20 repetitions are allowed!");
						this.error_label.setVisible(true);
					}

				} else {
					ok.requestFocus();
					this.error_label.setText("Please enter valid values!");
					this.error_label.setVisible(true);
				}

			} else {
				ok.requestFocus();
				this.error_label.setText("You must fill out all the fields!");
				this.error_label.setVisible(true);
			}

		} else {
			ok.requestFocus();
			this.error_label.setText("Please select your choice of game!");
			this.error_label.setVisible(true);
		}

	}

	/**
	 * Called when the admin pressed the Game ComboBox
	 * 
	 * @param evt
	 *            The ActionEvent called when the button is pressed
	 * @throws RemoteException
	 */
	public void GameIssueFired(ActionEvent evt) throws RemoteException {
		gameChoice = game.getValue();
	}

	/**
	 * Called when the admin press the AI ComboBox
	 * 
	 * @param evt
	 *            The ActionEvent called when the button is pressed
	 * @throws RemoteException
	 */
	public void AIIssueFired(ActionEvent evt) throws RemoteException {
		this.aiChoice = typeAI.getValue();
	}

	public int getNumberOfRepetitionsPerRoom() {
		return this.numberOfRepetitionsPerRoom;
	}

	public int getNumberOfUsersPerRoom() {
		return this.numberOfUsersPerRoom;
	}

	public double getAmountPerUser() {
		return this.amountPerUser;
	}
}