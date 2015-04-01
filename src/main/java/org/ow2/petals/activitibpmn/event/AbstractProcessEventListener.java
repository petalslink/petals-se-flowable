/**
 * Copyright (c) 2015 Linagora
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
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.event;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import org.activiti.engine.HistoryService;
import org.activiti.engine.delegate.event.ActivitiEvent;
import org.activiti.engine.delegate.event.ActivitiEventListener;
import org.activiti.engine.delegate.event.ActivitiEventType;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

public abstract class AbstractProcessEventListener implements ActivitiEventListener {

    protected final Logger log;

    protected final HistoryService historyService;

    private final ActivitiEventType listenEvent;

    private final ScheduledExecutorService scheduledLogger;

    private final int delay;

    public AbstractProcessEventListener(final ScheduledExecutorService scheduledLogger, final int delay,
            final ActivitiEventType listenEvent, final HistoryService historyService,
            final Logger log) {

        assert log != null;
        assert historyService != null;
        assert listenEvent != null;
        assert scheduledLogger != null;

        this.log = log;
        this.historyService = historyService;
        this.listenEvent = listenEvent;
        this.scheduledLogger = scheduledLogger;
        this.delay = delay;
    }

    protected abstract AbstractFlowLogData createLogData(final ActivitiEvent event);

    @Override
    public void onEvent(final ActivitiEvent event) {

        assert event != null;

        if (event.getType() == this.listenEvent) {
            
            this.scheduledLogger.schedule(new Runnable() {
                
                @Override
                public void run() {
                    AbstractProcessEventListener.this.log.log(Level.MONIT, "", AbstractProcessEventListener.this.createLogData(event));
                }
            }, this.delay, TimeUnit.MILLISECONDS);
            
            
        } else {
            log.warning("Unexpected event type '" + event.getType().name() + "'. Event discarded !");
        }

    }

    @Override
    public boolean isFailOnException() {
        return false;
    }

}
