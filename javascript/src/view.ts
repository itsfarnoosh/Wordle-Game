import { Constants } from "./utils";
import { State } from "./types";
import toastr from "toastr";
import { fromEvent, from, of, Observable } from "rxjs";
import { take, delay, mergeMap, map } from "rxjs/operators";
export function initBoard(): void {
    const board = document.getElementById("game-board");

    if (!board) {
        return;
    }

    Array.from({ length: Constants.NUMBER_OF_GUESSES }).forEach(() => {
        const row = Object.assign(document.createElement("div"), {
            className: "letter-row",
        });

        Array.from({ length: Constants.WORD_LENGTH }).forEach(() => {
            const box = document.createElement("div");
            box.className = "letter-box";
            row.appendChild(box);
        });

        board.appendChild(row);
    });
}

function shadeKeyBoard(letter: string, color: string): void {
    const elements = Array.from(
        document.getElementsByClassName("keyboard-button"),
    );

    const targetElement = elements.find(
        (elem) => elem.textContent === letter,
    ) as HTMLElement;

    if (targetElement) {
        const oldColor = targetElement.dataset.color;

        if (
            oldColor === "green" ||
            (oldColor === "yellow" && color !== "green")
        ) {
            return;
        }
        targetElement.dataset.color = color;
    }
}

const animateCSS = (
    element: HTMLElement,
    animation: string,
    prefix = "animate__",
): Observable<{ element: HTMLElement; animationName: string }> => {
    const animationName = `${prefix}${animation}`;

    element.style.setProperty("--animate-duration", "0.3s");
    element.classList.add(`${prefix}animated`, animationName);

    return fromEvent<AnimationEvent>(element, "animationend").pipe(
        take(1),
        map(() => ({ element, animationName })),
    );
};

export function render(state: State): void {
    if (state.currentGuesses <= Constants.NUMBER_OF_GUESSES - 1) {
        const row =
            document.getElementsByClassName("letter-row")[state.currentGuesses];

        Array.from(row.children).forEach((box, i) => {
            if (i >= state.currentGuess.length) {
                box.textContent = "";
                box.classList.remove("filled-box");
            } else {
                if (!box.classList.contains("filled-box")) {
                    animateCSS(box as HTMLElement, "pulse");
                }
                box.textContent = state.currentGuess[i];
                box.classList.add("filled-box");
            }
        });
    }
    if (state.complete) {
        if (state.success) {
            toastr.success("You guessed right! Game over!");
        } else {
            toastr.error("You've run out of guesses! Game over!");
            toastr.info(`The right word was: "${state.targetWord}"`);
        }
    }
    if (state.feedback !== null) {
        const prevRow =
            document.getElementsByClassName("letter-row")[
                state.currentGuesses - 1
            ];

        from(Array.from(prevRow.children))
            .pipe(
                mergeMap((box, i) => {
                    const boxElement = box as HTMLElement;
                    const boxDelay = 250 * i;

                    return of({ boxElement, i }).pipe(
                        delay(boxDelay), // Delay each box animation based on index
                        mergeMap(({ boxElement, i }) =>
                            animateCSS(boxElement, "flipInX").pipe(
                                map((x) => ({ ...x, i })),
                            ),
                        ),
                    );
                }),
            )
            .subscribe(({ element, animationName, i }) => {
                element.classList.remove(
                    `${animationName}`,
                    `animate__animated`,
                );

                if (state.feedback !== null) {
                    element.dataset.color = state.feedback[i];
                    shadeKeyBoard(
                        state.previousGuess!.charAt(i),
                        state.feedback[i],
                    );
                }
            });
    }

    if (state.error !== null) {
        toastr.error(state.error);
    }
}
