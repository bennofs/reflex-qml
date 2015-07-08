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
            width: childrenRect.width
            height: childrenRect.height

            Text {
                id: label
                text: modelData.description

                MouseArea {
                    id: mouse
                    onDoubleClicked: modelData.editing = true
                }
            }

            TextField {
                id: input
                anchors.fill: label
                visible: false
            }

            states:
            [ State {
                name: "EDITING"
                when: modelData.editing
                PropertyChanges { target: checkBox; visible: false }
                PropertyChanges { target: label; visible: false }
                PropertyChanges { target: input; visible: true }
            }
            ]
        }
    }
}