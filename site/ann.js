var modal, del_btn, close_btn;

function show_modal(del, beg, end) {
    document.getElementById('modal').style.display = "block";
    document.getElementById('modal-hid').value = beg + "," + end;
    if (del) {
        del_btn.disabled = false;
        del_btn.onclick = function () {
            rem_ann(del);
            hide_modal();
            return false;
        };
    }
}

function hide_modal() {
    modal.style.display = "none";
    del_btn.style.disabled = true;    
}

function rem_ann(i) {
    $.ajax({
        type: "POST",
        url: "/post/" + window.location.pathname.substr(5),
        data: JSON.stringify({
            action: "rem",
            id: i,
            dataType: "application/json"
        })
    }).done(function (data) {
        window.getSelection().empty();
        data = JSON.parse(data);
        $("#txt-data").html(data.txt);
        $("#ann-data").html(data.ann);
    });

    return false;
}

function ann_dialog(i, tag) {
    $("input").prop("checked", false);
    if (i) {
        var span = $("#ann-" + i);
        $("#radio-" + tag).prop("checked", true);
        show_modal(i, span.attr("beg"), span.attr("end"));
    } else {
        var sel = window.getSelection();
        
        if (sel.getRangeAt(0).toString() === "") {
            var span = $(event.target);
            if (span.is("span"))
                show_modal(Number(span.attr("id").substr(4)),
                           span.attr("beg"), span.attr("end"));
        } else {            
            var anchor = sel.anchorNode;
            var focus = sel.focusNode;
            var beg_off = 0;
            var end_off = 0;

            if ($(focus).parent().is("span") || $(anchor).parent().is("span")) {
                alert("Overlapping spans not supported. Sorry");
                return false;
            }

            var stop = false;
            $(anchor).parent().contents().each(function(i, el) {
                if (!stop) {
                    if (anchor == el)
                        stop = true;
                    else
                        beg_off += $(el).text().length;
                }
            });

            var stop = false;
            $(focus).parent().contents().each(function(i, el) {
                if (!stop) {
                    if (focus == el)
                        stop = true;
                    else
                        end_off += $(el).text().length;
                }
            });

            console.log((sel.anchorOffset + beg_off) + " " +
                        (sel.focusOffset + end_off));
            
            show_modal(false,
                       sel.anchorOffset + beg_off,
                       sel.focusOffset + end_off);
        }
    }
    
    return false;
}

function post_ann() {
    var checked = $('input:checked');

    $.ajax({
        type: "POST",
        url: "/post/" + window.location.pathname.substr(5),
        data: JSON.stringify({
            action: "add",
            span: document.getElementById('modal-hid').value,
            tag: $("input:checked")[0]
                ? $("input:checked")[0].value
                : $("input.radio")[0].value,
            dataType: "application/json"
        })
    }).done(function (data) {
        hide_modal();
        data = JSON.parse(data);
        $("#txt-data").html(data.txt);
        $("#ann-data").html(data.ann);
    });
    
    return false;
}


$(function () {
    modal = document.getElementById("modal");
    del_btn = document.getElementById("modal-del");
    close_btn = document.getElementsByClassName("close")[0];
        
    close_btn.onclick = hide_modal;
    
    window.onclick = function(event) {
        if (event.target == modal) {
            hide_modal();
        }
    }

    window.onkeydown = function(event) {
        var ESCAPE_CODE = 27;
        var LEFT_ARROW = 37;
        var RIGHT_ARROW = 39;
        if (event.keyCode == LEFT_ARROW) {
            document.querySelectorAll('.file-nav a')[0].click()
        }
        if (event.keyCode == RIGHT_ARROW) {
            document.querySelectorAll('.file-nav a')[1].click()
        }
        if (event.keyCode == ESCAPE_CODE && modal.style.display == "block") {
            hide_modal();
        }
    };
});
