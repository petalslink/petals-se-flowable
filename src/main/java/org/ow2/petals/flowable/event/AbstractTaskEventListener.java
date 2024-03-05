/**
 * Copyright (c) 2017-2024 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.event;

import java.util.logging.Logger;

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.engine.TaskService;

/**
 * Base class for listeners about task events
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class AbstractTaskEventListener extends AbstractMonitDirectLoggerEventListener {

    protected final TaskService taskService;

    public AbstractTaskEventListener(final FlowableEngineEventType listenEventType, final TaskService taskService,
            final Logger log) {
        super(listenEventType, log);
        this.taskService = taskService;
    }
}
