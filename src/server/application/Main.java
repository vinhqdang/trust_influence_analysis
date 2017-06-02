package server.application;

import java.io.IOException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;
import server.utilities.ServerRMI;

import common.IServerRMI;

public class Main extends Application {

	private FrameServerController serverController;
	private ServerRMI server;

	@Override
	public void start(Stage primaryStage) {
		try {
			server = new ServerRMI();
			IServerRMI skeleton = (IServerRMI) UnicastRemoteObject
					.exportObject(server, 10000);
			Registry registry = LocateRegistry.createRegistry(10000);
			registry.rebind("Add", skeleton);

			FXMLLoader loader = new FXMLLoader(getClass().getResource(
					"FrameServer.fxml"));
			serverController = new FrameServerController(server);
			loader.setController(serverController);

			AnchorPane root = (AnchorPane) loader.load();
			Scene scene = new Scene(root, 800, 600);
			scene.getStylesheets().add(
					getClass().getResource("application.css").toExternalForm());
			primaryStage.setScene(scene);
			primaryStage.show();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public static void main(String[] args) throws IOException {
		launch(args);
	}
}
