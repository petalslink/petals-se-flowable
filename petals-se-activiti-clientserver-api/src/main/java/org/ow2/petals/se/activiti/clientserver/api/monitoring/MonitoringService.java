/**
 * Copyright (c) 2016 Linagora
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
package org.ow2.petals.se.activiti.clientserver.api.monitoring;

import java.util.Map;

import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringProbeNotInitializedException;
import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringProbeNotStartedException;
import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringServiceException;

/**
 * Client/Server interface of the service 'Monitoring' part dedicated to the SE Activiti
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public interface MonitoringService
        extends org.ow2.petals.component.framework.clientserver.api.monitoring.MonitoringService {

    //
    // --- Process definitions / Process instances
    //

    /**
     * <p>
     * Returns information on process definitions.</>
     * <p>
     * The map key is the process definition identifier. Each value of the map entry is an array of numberes where:
     * <ul>
     * <li>the 1st value is the suspension state: {@code 1} the process definition is suspended, {@code 0} it is not
     * suspended,
     * <li>
     * <li>the 2nd value is the number of active process instances,</li>
     * <li>the 3rd value is the number of suspended process instances,</li>
     * <li>the 4th value is the number of ended process instances.</li>
     * </ul>
     * </p>
     */
    public Map<String, Long[]> getProcessDefinitions();

    /**
     * @return The current max size of the asynchronous executor thread pool.
     * @throws MonitoringServiceException
     */
    public long getAsyncExecutorThreadPoolMaxSize() throws MonitoringServiceException;

    /**
     * @return The current min size of the asynchronous executor thread pool.
     * @throws MonitoringServiceException
     */
    public long getAsyncExecutorThreadPoolMinSize() throws MonitoringServiceException;

    /**
     * @return The max number of active threads of the asynchronous executor thread pool.
     * @throws MonitoringProbeNotInitializedException
     *             The probe is not initialized or shutdown.
     * @throws MonitoringServiceException
     */
    public long getAsyncExecutorThreadPoolActiveThreadsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException;

    /**
     * @return The current number of active threads of the asynchronous executor thread pool.
     * @throws MonitoringProbeNotStartedException
     *             The probe is not started or stopped.
     * @throws MonitoringServiceException
     */
    public long getAsyncExecutorThreadPoolActiveThreadsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException;

    /**
     * @return The max number of idle threads of the asynchronous executor thread pool.
     * @throws MonitoringProbeNotInitializedException
     *             The probe is not initialized or shutdown.
     * @throws MonitoringServiceException
     */
    public long getAsyncExecutorThreadPoolIdleThreadsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException;

    /**
     * @return The current number of idle threads of the asynchronous executor thread pool.
     * @throws MonitoringProbeNotStartedException
     *             The probe is not started or stopped.
     * @throws MonitoringServiceException
     */
    public long getAsyncExecutorThreadPoolIdleThreadsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException;

    /**
     * @return The max number of enqueued tasks into the asynchronous executor thread pool.
     * @throws MonitoringProbeNotInitializedException
     *             The probe is not initialized or shutdown.
     * @throws MonitoringServiceException
     */
    public long getAsyncExecutorThreadPoolQueuedRequestsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException;

    /**
     * @return The current number of enqueued tasks into the asynchronous executor thread pool.
     * @throws MonitoringProbeNotStartedException
     *             The probe is not started or stopped.
     * @throws MonitoringServiceException
     */
    public long getAsyncExecutorThreadPoolQueuedRequestsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException;

}
