// https://www.freecodecamp.org/news/build-a-wordle-clone-in-javascript/
// Web page adapted from here

import { map, filter, takeWhile, concatMap, take } from "rxjs/operators";
import { fromEvent, merge, mergeScan, type Observable, of } from "rxjs";

import { render, initBoard } from "./view";
import { State, initialState, Action, nullify } from "./types";
import { Enter, Delete, Insert, CreateWord, wordCreator } from "./state";

function main() {
    const keyboardCont = document.getElementById("keyboard-cont")!;

    // Convert the keyboard click to the equivalent mouse event!
    const keyboardMouse$ = fromEvent(keyboardCont, "click").pipe(
        filter((e) => {
            const target = e.target as HTMLElement;
            return target.classList.contains("keyboard-button");
        }),
        map((e) => {
            const target = e.target as HTMLElement;
            const tc = target.textContent;
            const key =
                tc === null ? undefined : tc === "Del" ? "Backspace" : tc;
            return new KeyboardEvent("keyup", { key });
        }),
    );

    // Merge Actual keys with keyboard clicks
    const keyup$ = merge(
        keyboardMouse$,
        fromEvent<KeyboardEvent>(document, "keyup"),
    );

    function createActionStream(
        regex: RegExp,
        ActionClass: new (key: string) => Action,
    ): Observable<Action> {
        return keyup$.pipe(
            filter((e) => regex.test(String(e.key))),
            map((e) => new ActionClass(String(e.key))),
        );
    }

    // Create our streams
    const actions: ReadonlyArray<[RegExp, new (key: string) => Action]> = [
        [/^Backspace$/, Delete],
        [/^Enter$/, Enter],
        [/^[a-z]$/, Insert],
    ];
    const action$ = actions.map(([x, y]) => createActionStream(x, y));
    const reduceState = (s: State, action: Action) => action.apply(s);

    wordCreator()
        .pipe(
            take(1),
            map((x) => new CreateWord(x)),
            concatMap((targetWord) => merge(of(targetWord), ...action$)),
        )
        .pipe(
            mergeScan((acc, value) => {
                // Turn current action in to an observable and mergeScan in to the stream
                return reduceState(nullify(acc), value);
            }, initialState),
            takeWhile((s) => !s.complete, true),
        )
        .subscribe(render);
}

// The following simply runs your main function on window load. Make sure to leave it in place.
if (typeof window !== "undefined") {
    document.addEventListener("DOMContentLoaded", () => {
        initBoard();
        main();
    });
}
