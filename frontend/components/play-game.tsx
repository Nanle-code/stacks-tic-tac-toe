"use client";

import { Game, Move, canCancelGame, CancelCheckResponse, blocksToTime } from "@/lib/contract";
import { GameBoard } from "./game-board";
import { abbreviateAddress, explorerAddress, formatStx } from "@/lib/stx-utils";
import Link from "next/link";
import { useStacks } from "@/hooks/use-stacks";
import { useEffect, useState } from "react";

interface PlayGameProps {
  game: Game;
}

export function PlayGame({ game }: PlayGameProps) {
  const { userData, handleJoinGame, handlePlayGame, handleCancelGameTimeout } = useStacks();
  
  const [board, setBoard] = useState(game.board);
  const [playedMoveIndex, setPlayedMoveIndex] = useState(-1);
  
  // NEW: State for cancel check
  const [cancelCheck, setCancelCheck] = useState<CancelCheckResponse | null>(null);

  // NEW: Poll for cancel status every 30 seconds
  useEffect(() => {
    async function checkCancelStatus() {
      if (!userData || game.winner) return;
      
      const result = await canCancelGame(game.id);
      setCancelCheck(result);
    }

    checkCancelStatus();
    const interval = setInterval(checkCancelStatus, 30000); // Check every 30 seconds

    return () => clearInterval(interval);
  }, [game.id, userData, game.winner]);

  if (!userData) return null;

  const isPlayerOne = userData.profile.stxAddress.testnet === game["player-one"];
  const isPlayerTwo = userData.profile.stxAddress.testnet === game["player-two"];
  
  const isJoinable = game["player-two"] === null && !isPlayerOne;
  const isJoinedAlready = isPlayerOne || isPlayerTwo;
  const nextMove = game["is-player-one-turn"] ? Move.X : Move.O;
  const isMyTurn =
    (game["is-player-one-turn"] && isPlayerOne) ||
    (!game["is-player-one-turn"] && isPlayerTwo);
  const isGameOver = game.winner !== null;
  
  // NEW: Check if current user can cancel
  const canCancel = cancelCheck?.canCancel && 
    isJoinedAlready && 
    !isMyTurn && 
    !isGameOver;

  function onCellClick(index: number) {
    const tempBoard = [...game.board];
    tempBoard[index] = nextMove;
    setBoard(tempBoard);
    setPlayedMoveIndex(index);
  }

  return (
    <div className="flex flex-col gap-4 w-[400px]">
      <GameBoard
        board={board}
        onCellClick={onCellClick}
        nextMove={nextMove}
        cellClassName="size-32 text-6xl"
      />

      <div className="flex flex-col gap-2">
        <div className="flex items-center justify-between gap-2">
          <span className="text-gray-500">Bet Amount: </span>
          <span>{formatStx(game["bet-amount"])} STX</span>
        </div>

        <div className="flex items-center justify-between gap-2">
          <span className="text-gray-500">Player One: </span>
          <Link
            href={explorerAddress(game["player-one"])}
            target="_blank"
            className="hover:underline"
          >
            {abbreviateAddress(game["player-one"])}
          </Link>
        </div>

        <div className="flex items-center justify-between gap-2">
          <span className="text-gray-500">Player Two: </span>
          {game["player-two"] ? (
            <Link
              href={explorerAddress(game["player-two"])}
              target="_blank"
              className="hover:underline"
            >
              {abbreviateAddress(game["player-two"])}
            </Link>
          ) : (
            <span>Nobody</span>
          )}
        </div>

        {game["winner"] && (
          <div className="flex items-center justify-between gap-2">
            <span className="text-gray-500">Winner: </span>
            <Link
              href={explorerAddress(game["winner"])}
              target="_blank"
              className="hover:underline"
            >
              {abbreviateAddress(game["winner"])}
            </Link>
          </div>
        )}

        {/* NEW: Timeout information */}
        {!isGameOver && cancelCheck && game["player-two"] && (
          <div className="flex items-center justify-between gap-2">
            <span className="text-gray-500">Timeout: </span>
            <span className="text-sm">
              {cancelCheck.canCancel 
                ? "Available to cancel" 
                : `${blocksToTime(cancelCheck.blocksUntilTimeout)} remaining`}
            </span>
          </div>
        )}
      </div>

      {isJoinable && (
        <button
          onClick={() => handleJoinGame(game.id, playedMoveIndex, nextMove)}
          className="bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600"
        >
          Join Game
        </button>
      )}

      {isMyTurn && (
        <button
          onClick={() => handlePlayGame(game.id, playedMoveIndex, nextMove)}
          className="bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600"
        >
          Play
        </button>
      )}

      {/* NEW: Cancel button */}
      {canCancel && (
        <div className="flex flex-col gap-2">
          <div className="text-sm text-yellow-500 bg-yellow-500/10 p-3 rounded border border-yellow-500/20">
            ⚠️ Your opponent has been inactive for over 24 hours. You can cancel the game and claim both bets.
          </div>
          <button
            onClick={() => handleCancelGameTimeout(game.id)}
            className="bg-red-500 text-white px-4 py-2 rounded hover:bg-red-600 font-semibold"
          >
            Cancel Game & Claim Funds
          </button>
        </div>
      )}

      {isJoinedAlready && !isMyTurn && !isGameOver && !canCancel && (
        <div className="text-gray-500">Waiting for opponent to play...</div>
      )}
    </div>
  );
}