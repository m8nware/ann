var to_remove = [];
var cat_changes = {};

function position(elementToFind, arrayElements) {
    var i;
    for (i = 0; i < arrayElements.length; i += 1) {
        if (JSON.stringify(arrayElements[i]) == JSON.stringify(elementToFind)) {
            return i;
        }
    }
    return -1; //not found
}

function rem_diff(i, ann) {
    var pos = position([i, ann], to_remove);
    if (pos != -1)
        to_remove.splice(pos, 1);
    else
        to_remove.push([i, ann]);
    $(event.target).parent().toggleClass("removed");
    return false;
}

function post_diff(path) {
    $.ajax({
        type: "POST",
        url: "/diff/" + path,
        data: JSON.stringify([to_remove, cat_changes])
    }).done(function (data) {
        alert(data);
    });

    return false;
}

function edit_cat(id, old) {
    var cur = $.trim($("#cat-" + id).val());
    if (cur != old) {
        cat_changes[id] = cur;
    }
}


function highlight (beg, end) {
    var text = $("#txt-data");
    var prev = text.text();
    text.html(prev.substring(0, beg) +
              "<span id='tmp-focus' style='background-color: yellow'>" +
              prev.substring(beg, end) + "</span>" + prev.substring(end));

    var cur_off = $('html, body').scrollTop();
    var offset = $("#tmp-focus").offset();
    offset.top -= 100;
    $('html, body').animate({
        scrollTop: offset.top
    });
    
    setTimeout(function () {
        text.html(prev);
            $('html, body').animate({
                scrollTop: cur_off
            });
    }, 50 * (end - beg));
    return false;
}
