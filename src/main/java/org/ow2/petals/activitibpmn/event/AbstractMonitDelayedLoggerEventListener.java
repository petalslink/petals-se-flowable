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

import org.activiti.engine.delegate.event.ActivitiEventType;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;

/**
 * Abstract class logging a delayed MONIT trace on Activiti events
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public abstract class AbstractMonitDelayedLoggerEventListener extends AbstractMonitLoggerEventListener {

    private final ScheduledExecutorService scheduledLogger;

    private final int delay;

    public AbstractMonitDelayedLoggerEventListener(final ScheduledExecutorService scheduledLogger, final int delay,
            final ActivitiEventType listenEvent, final Logger log) {

        super(listenEvent, log);

        assert scheduledLogger != null;

        this.scheduledLogger = scheduledLogger;
        this.delay = delay;
    }

    @Override
    protected void flushLogData(final AbstractFlowLogData logData) {

        this.scheduledLogger.schedule(new Runnable() {
            
            @Override
            public void run() {
                AbstractMonitDelayedLoggerEventListener.this.log.log(Level.MONIT, "", logData);
            }
        }, this.delay, TimeUnit.MILLISECONDS);
    }

}
