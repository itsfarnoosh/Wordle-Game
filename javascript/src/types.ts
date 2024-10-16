import type { Observable } from "rxjs";

export type State = Readonly<{
    currentGuesses: number;
    targetWord: string;
    currentGuess: string;
    previousGuess: string | null;
    complete: boolean;
    success: boolean;
    feedback: readonly string[] | null;
    error: string | null;
}>;

export const initialState: State = {
    currentGuesses: 0,
    targetWord: "press",
    currentGuess: "",
    complete: false,
    success: false,
    feedback: null,
    error: null,
    previousGuess: null,
};

export const nullify = (s: State): State => ({
    ...s,
    feedback: null,
    error: null,
});

export interface Action {
    apply: (s: State) => Observable<State>;
}
