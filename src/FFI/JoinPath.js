import path from 'path';

export const joinPath = (start) => (end) => path.join(start, end);
