namespace FSharp.Control

open System
open Core

/// Overloads and extensions for AsyncDisposable
type AsyncDisposable (cancel) =
    interface IAsyncRxDisposable with
        member this.DisposeAsync () =
            cancel ()

    static member Create (cancel) : IAsyncRxDisposable =
        AsyncDisposable cancel :> IAsyncRxDisposable

    static member Empty : IAsyncRxDisposable =
        let cancel () = async {
            return ()
        }
        AsyncDisposable cancel :> IAsyncRxDisposable

    static member Composite (disposables: IAsyncRxDisposable seq) : IAsyncRxDisposable =
        let cancel () = async {
            for d in disposables do
                do! d.DisposeAsync ()
        }
        AsyncDisposable cancel :> IAsyncRxDisposable

type Disposable (cancel) =
    interface IDisposable with
        member this.Dispose () =
            cancel ()

    static member Create (cancel) : IDisposable =
        new Disposable(cancel) :> IDisposable

    static member Empty : IDisposable =
        let cancel () =
            ()

        new Disposable(cancel) :> IDisposable

    static member Composite (disposables: IDisposable seq) : IDisposable =
        let cancel () =
            for d in disposables do
                d.Dispose ()

        new Disposable (cancel) :> IDisposable

[<AutoOpen>]
module AsyncDisposable =
    type IAsyncRxDisposable with
        member this.ToDisposable () =
            { new IDisposable with member __.Dispose () = this.DisposeAsync () |> Async.Start' }

    type System.IDisposable with
        member this.ToAsyncDisposable () : IAsyncRxDisposable =
            { new IAsyncRxDisposable with member __.DisposeAsync () = async { this.Dispose () } }
