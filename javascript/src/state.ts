import { ajax } from "rxjs/ajax";
import { map } from "rxjs/operators";
import { of, type Observable } from "rxjs";

import { Constants } from "./utils";
import { State, Action } from "./types";

export function wordCreator(): Observable<string> {
    // Get the target word as a stream! Only should be one word
    return ajax<{ target: string }>({
        url: "/api/getTarget",
        method: "GET",
        body: "",
    }).pipe(
        map((response) => response.response), // Extracting the response data
        map((data) => {
            return data.target;
        }),
    );
}

export class Insert implements Action {
    constructor(public readonly letter: string) {}
    apply(s: State): Observable<State> {
        if (s.currentGuess.length === Constants.WORD_LENGTH) {
            return of(s);
        }
        return of({
            ...s,
            currentGuess: s.currentGuess + this.letter,
        });
    }
}

export class Enter implements Action {
    constructor() {}
    apply(s: State): Observable<State> {
        // Create a stream for communication to back-end about state
        if (s.currentGuess.length !== Constants.WORD_LENGTH) {
            return of({ ...s, error: "Not enough letters!" });
        }
        return ajax<{ feedback: string; valid: string }>({
            url: "/api/makeGuess",
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                guess: s.currentGuess,
                target: s.targetWord,
                previousGuess: s.previousGuess,
            }),
        }).pipe(
            map((response) => response.response), // Extracting the response data
            map((data): State => {
                if (data.valid === "False") {
                    return { ...s, error: "Invalid Guess" };
                }
                const correctGuess = s.targetWord === s.currentGuess;
                return {
                    ...s,
                    feedback: JSON.parse(data.feedback),
                    success: correctGuess,
                    complete:
                        correctGuess ||
                        s.currentGuesses === Constants.WORD_LENGTH,
                    currentGuesses: s.currentGuesses + 1,
                    previousGuess: s.currentGuess,
                    currentGuess: "",
                };
            }),
        );
    }
}

export class Delete implements Action {
    apply(s: State): Observable<State> {
        return of({ ...s, currentGuess: s.currentGuess.slice(0, -1) });
    }
}

export class CreateWord implements Action {
    constructor(public readonly word: string) {}
    apply(s: State): Observable<State> {
        return of({ ...s, targetWord: this.word });
    }
}
