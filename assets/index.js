import './styles.css';
import './live_reload.js';
import { angular } from '@angular-wave/angular.ts';
import { configureBattleship } from './application.js';

configureBattleship(angular);
angular.bootstrap(document, ['battleship']);
