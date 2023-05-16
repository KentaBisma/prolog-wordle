var currentWord = [];
var currentIndex = 0;

function backspace(c) {
    return c === "⟵" ? "backspace" : c;
}

$(() => {
    // Populate the keys
    ["QWERTYUIOP⟵", "ASDFGHJKL", "ZXCVBNM"].forEach((row) => {
        let buttons = [...row]
            .map(
                (c) => `<button class="key" id="key-${backspace(c)}">${c}</button>`
            )
            .join("");
        $("#keyboard").append(`<div class="keyboard-row">${buttons}</div>`);
        [...row].forEach((c) => {
            $(`#key-${backspace(c)}`).click(() => put(c));
        });
    });

    document
        .getElementById("hint-button")
        .addEventListener("keyup", function (event) {
            if (event.key === "Enter") {
                event.preventDefault(); // Prevent the default button click behavior
            }
        });


    document.addEventListener("keydown", (e) => {
        if (
            [..."abcdefghijklmnopqrstuvwxyz", "Backspace", "Enter"].includes(e.key)
        ) {
            switch (e.key) {
                case "Backspace":
                    put("⟵");
                    break;
                case "Enter":
                    if (!Boolean($("#button-submit").attr("disabled"))) validate();
                    break;
                default:
                    if (!Boolean($(`#key-${e.key.toUpperCase()}`).attr("disabled")))
                        put(e.key.toUpperCase());
            }
        }
    });
});

function put(c) {
    if (c === "⟵") {
        currentWord.pop();
        redBoxes(false);
    } else if (currentWord.length != 5) {
        currentWord.push(c);
    }
    updateLetters();
    if (currentWord.length == 5) {
        $.ajax({
            method: "get",
            url: `/check/?word=${currentWord.join("").toLowerCase()}`,
        }).then((res) => {
            if (!res.wordExists) {
                redBoxes(true);
                $("#button-submit").attr("disabled", true);
            } else {
                redBoxes(false);
                $("#button-submit").attr("disabled", false);
            }
        });
    } else {
        $("#button-submit").attr("disabled", true);
    }
}

function validate() {
    $.ajax({
        method: "get",
        url: `/validate/?word=${currentWord.join("").toLowerCase()}`,
    }).then((res) => {
        if (!res.result) return;
        if (!res.result.startsWith) {
            updateBoxes(res.result);
            updateHints();
        } else {
            let unpacked = unpackMessage(res.result);
            if (unpacked.status === "win") {
                updateBoxes(["green", "green", "green", "green", "green"]);
            } else {
                redBoxes(true);
            }
            showAnswer(unpacked.answer);
            updateStatus(unpacked.status);
            freezeKeyboard();
        }
        currentWord = [];
        currentIndex++;
        if (currentIndex >= 5) currentIndex = 0;
        $("#button-submit").attr("disabled", true);
    });
}

function updateHints() {
    $.ajax({
        method: "get",
        url: `/hints`,
    }).then((res) => {
        let open = $("#hints").hasClass("w-full");
        if (res.validWords) {
            generateHints('valid-words', res.validWords)
        }
        if (res.solutions) {
            generateHints('solutions', res.solutions)
        }
    });
}

function generateHints(id, list) {
    let spans = "";
    for (let w of list) {
        spans += `<span class="word ${open ? "" : "hidden"
            }">${w.toUpperCase()}</span>`;
    }
    $(`#${id}`).html(spans);
    $(`#${id}`).addClass("grid grid-cols-5");
}

function freezeKeyboard() {
    $(".key").add("#button-submit").attr("disabled", true);
}

function unpackMessage(message) {
    [status, answer] = [...message.slice(8, -1).split(",")].map((c) =>
        c.trim()
    );
    return { status, answer };
}

function updateLetters() {
    for (let i = 0; i < 5; i++) {
        $(`#b${currentIndex}${i}`).html(
            i < currentWord.length ? currentWord[i] : ""
        );
    }
}

function showAnswer(answer) {
    for (let i = 0; i < 5; i++) {
        $(`#a0${i}`).html(answer.charAt(i).toUpperCase());
    }
}

function updateStatus(status) {
    $("#gameover").removeClass(["opacity-0", "pointer-events-none"]);
    $("#status").html(`You ${status}!`);
}

function redBoxes(add) {
    for (let i = 0; i < 5; i++) {
        let boxes = $(`#b${currentIndex}${i}`);
        if (add) boxes.addClass("red");
        else boxes.removeClass("red");
    }
}

function updateBoxes(arr) {
    for (let i = 0; i < 5; i++) {
        $(`#b${currentIndex}${i}`).addClass(arr[i]);
    }
}

function toggleHints() {
    let open = $("#hints")
        .toggleClass(["w-0", "overflow-hidden", "w-full"])
        .hasClass("w-full");
    if (open) $(".word").removeClass(["hidden"]);
    else $(".word").addClass(["hidden"]);
}

function toggleModal(open) {
    if (open) $("#modal").removeClass(["hidden"]).addClass("flex");
    else $("#modal").addClass(["hidden"]).removeClass("flex");
}

function toggleHowToPlay() {
    toggleModal(true);
    $("#modal-title").html("How to play?");
    $("#modal-content").html(`
    Guess the Wordle in 5 tries. </br>
    </br>
    Each guess must be a valid 5-letter word.
    The color of the tiles will change to show how close your guess was to the word.</br>
    </br>
    🟩 means the letter is in the word and is in the right spot.</br>
    🟨 means the letter is in the word but is not in the right spot.</br>
    ⬛ means the letter is not in the word.</br>
    `);
}

function toggleCredits() {
    toggleModal(true);
    $("#modal-title").html("Credits");
    $("#modal-content").html(`
    The game is based on the game 'Wordle' by Josh Wardle. </br>
    </br>
    The words used are collected from <a class="text-gray-400" href="https://dagshub.com/arjvik/wordle-wordlist">this</a> database. </br></br>
    <a class="text-gray-400" href="https://github.com/KentaBisma/prolog-wordle">Sourcecode</a> </br>
    `);
}