import QtQuick 2.0

Item {
    property var todos: [
      { description: "Finish this app!", completed: false, editing: false },
      { description: "Publish the app!", completed: false, editing: false },
      { description: "Write docs", completed: false, editing: false },
      { description: "Add static model", completed: true, editing: false }
    ]
    property var itemsLeft: 4
}
