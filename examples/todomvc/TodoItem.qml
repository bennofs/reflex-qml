import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Controls.Styles 1.2
import QtQuick.Layouts 1.1

Item {
    id: root
    height: childrenRect.height
    Layout.minimumWidth: layout.Layout.minimumWidth

    RowLayout {
        id: layout
        width: parent.width

        CheckBox {
            id: checkBox
            style: CheckBoxStyle { spacing: 10 }
            checked: modelData.completed
            onClicked: modelData.completed = checked
        }

        Item {
            Layout.fillWidth: true
            Layout.minimumWidth: label.width
            height: input.height

            TextField {    
                id: input
                font.pixelSize: label.font.pixelSize
                width: parent.width
                text: modelData.description
                onAccepted: { modelData.description = text; modelData.editing = false; }
                opacity: 0
            }

            Text {
                id: label
                text: modelData.description
                height: parent.height
                verticalAlignment: Qt.AlignVCenter

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

        Button {
            text: "Remove"
            onClicked: modelData.remove()
        }

    }
}