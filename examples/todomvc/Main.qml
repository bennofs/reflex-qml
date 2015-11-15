import QtQuick 2.1
import QtQuick.Window 2.1
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Window {
    id: window
    color: "lightgray"
    title: "TodoMVC: reflex + QML"
    Component.onCompleted: window.show()

    ColumnLayout {
        anchors {
            fill: parent
            leftMargin: 15
            rightMargin: 15
            bottomMargin: 10
            topMargin: 5
        }
        Text {
            id: heading
            text: "todos"
            anchors.top: parent.top
            font.pointSize: 42
            color: "steelblue"
        }

        Rectangle {
            color: "gray"
            anchors.left: parent.left
            anchors.right: parent.right
            anchors.leftMargin: 2
            anchors.rightMargin: 2
            height: 1
        }

        TextField {
            Layout.fillWidth: true           
            id: new_todo_input
            validator: RegExpValidator {
                regExp: /.+/
            }
            onAccepted: {
                app.newItem(text);
                text = "";
            }
        }

        ListView {
            Layout.minimumHeight: 200
            Layout.fillHeight: true
            Layout.fillWidth: true
            model: app.todos
            delegate: TodoItem {
                width: parent.width
            }
        }

        RowLayout {
            Layout.fillWidth: true

            Text {
                text: app.itemsLeft + " items left"
            }

            Item {
                Layout.fillWidth: true
                height: childrenRect.height
                RowLayout {
                    anchors.horizontalCenter: parent.horizontalCenter
                    
                    Button {
                        text: "all"
                        onClicked: app.filter.all()
                    }
                    
                    Button {
                        text: "active"
                        onClicked: app.filter.active()
                    }
                    
                    Button {
                        text: "completed"
                        onClicked: app.filter.completed()
                    }
                }
            }

            Button {
                text: "Clear completed"
                onClicked: app.clearCompleted()
            }
        }
    }   
}
