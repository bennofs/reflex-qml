import QtQuick 2.2
import QtQuick.Controls 1.1

Rectangle {
    color: "lightgray"
    anchors.fill: parent

    Text {
        id: pageNumber
        text: "page: " + app.page        
        anchors.topMargin: 10
        anchors.leftMargin: 10
        anchors.top: parent.top
        anchors.left: parent.left
    }

    Column {
        id: table
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.leftMargin: 20
        anchors.top: pageNumber.bottom
        anchors.topMargin: 10

        Repeater {
            model: app.currentState     
            delegate: TextInput {
                id: input
                text: modelData.value
                font.pointSize: modelData.value * 5
                Behavior on font.pointSize {
                    NumberAnimation {
                        duration: 1000
                    }
                }

                onTextChanged: {
                    modelData.value = text == "" ? 0 : parseInt(text)
                    console.log("text changed: " + index + ":" + text)
                }
                property var computedColor: "black"
                color: computedColor
            }

        }
    }


    Row {
        anchors.top: table.bottom
        anchors.topMargin: 10  
        anchors.left: parent.left
        anchors.right: parent.right
        Text {
            text: "Add: "
            height: parent.height
        }
        Rectangle {
            color: "yellow"
            TextInput {
                id: entry
                width: parent.width
            }
            height: parent.height
            width: 100
        }
        Button {
            text: "Add"
            onClicked: app.addEntry(parseInt(entry.text == "" ? "0" : entry.text))
        }
    }
    
    Slider {
        minimumValue: 0
        maximumValue: 4
        stepSize: 1
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom
        value: app.page
        onValueChanged: app.page = value
    }
}