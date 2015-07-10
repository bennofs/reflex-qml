import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Controls.Styles 1.2

Item {
    id: root
    height: childrenRect.height
    width: childrenRect.width

    Row {
        CheckBox {
            id: checkBox
            checked: modelData.completed
            style: CheckBoxStyle { spacing: 10 } 
        }

        Item {
            width: 500
            height: input.height

            TextField {    
                id: input
                font.pixelSize: label.font.pixelSize
                anchors.fill: parent
                text: modelData.description
                onAccepted: { modelData.description = text; modelData.editing = false; }
                opacity: 0
            }

            Text {
                id: label
                text: modelData.description

                MouseArea {
                    id: mouse
                    anchors.fill: parent
                    onDoubleClicked: {
                        modelData.editing = true;
                    }
                }
            }

            states:
            [ State {
                name: "EDITING"
                when: modelData.editing
                PropertyChanges { target: checkBox; opacity: 0 }
                PropertyChanges { target: label; opacity: 0 }
                PropertyChanges { target: input; opacity: 1 }
            }
            ]
        }
    }
}