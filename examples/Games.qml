import QtQuick 2.1

Rectangle {
    Column {
        TextEdit {
            text: app.games.won
            onTextChanged: app.games.won = text == "" ? 0 : parseInt(text);
        }
        TextEdit {
            text: app.games.lost
            onTextChanged: app.games.lost = text == "" ? 0 : parseInt(text);
        }
        TextEdit {
            text: app.games.total
        }
    }
}