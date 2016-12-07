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
package org.ow2.petals.activitibpmn.monitoring;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.concurrent.ThreadPoolExecutor;

import org.activiti.engine.HistoryService;
import org.activiti.engine.ProcessEngine;
import org.activiti.engine.RepositoryService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.history.HistoricProcessInstance;
import org.activiti.engine.repository.ProcessDefinition;
import org.activiti.engine.runtime.ProcessInstance;
import org.ow2.petals.activitibpmn.monitoring.defect.AsyncExecutorThreadPoolDefectCreator;
import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringProbeNotInitializedException;
import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringProbeNotStartedException;
import org.ow2.petals.component.framework.clientserver.api.monitoring.exception.MonitoringServiceException;
import org.ow2.petals.component.framework.monitoring.defect.JmxDefectCreator;
import org.ow2.petals.probes.api.MacroProbesFactory;
import org.ow2.petals.probes.api.MacroProbesFactoryBuilder;
import org.ow2.petals.probes.api.exceptions.MultipleProbesFactoriesFoundException;
import org.ow2.petals.probes.api.exceptions.NoProbesFactoryFoundException;
import org.ow2.petals.probes.api.exceptions.ProbeInitializationException;
import org.ow2.petals.probes.api.exceptions.ProbeInitializedException;
import org.ow2.petals.probes.api.exceptions.ProbeNotInitializedException;
import org.ow2.petals.probes.api.exceptions.ProbeNotStartedException;
import org.ow2.petals.probes.api.exceptions.ProbeShutdownException;
import org.ow2.petals.probes.api.exceptions.ProbeStartedException;
import org.ow2.petals.probes.api.exceptions.ProbeStartupException;
import org.ow2.petals.probes.api.exceptions.ProbeStopException;
import org.ow2.petals.probes.api.probes.macro.ThreadPoolProbe;

/**
 * The monitoring MBean of the SE Activiti
 * 
 * @author Christophe DENEUX - Linagora
 */
public class Monitoring extends org.ow2.petals.component.framework.monitoring.Monitoring implements MonitoringMBean {

    /**
     * The repository service of the Activiti engine
     */
    private RepositoryService repositoryService = null;

    /**
     * The runtime service of the Activiti engine
     */
    private RuntimeService runtimeService = null;

    /**
     * The history service of the Activiti engine
     */
    private HistoryService historyService = null;

    // --- Probes --- //

    /**
     * The macro probe about the thread pool of the async executor.
     */
    private final ThreadPoolProbe probeAsyncExecutorThreadPool;

    // --- Defect creators --- //

    /**
     * The defect creator associated to the defect 'exhaustion of the async executor thread pool'.
     */
    private JmxDefectCreator asyncExecutorThreadPoolDefectCreator;

    /**
     * Creates the monitoring MBean
     * 
     * @param responseTimeProbesTimer
     *            The timer used as sampler by response time probes. <b>The caller is responsible to cancel the timer to
     *            free resources attached</b>.
     * @param samplePeriod
     *            The duration of a sample, in milliseconds.
     * @throws NoProbesFactoryFoundException
     *             No probe implementation found in the classloader
     * @throws MultipleProbesFactoriesFoundException
     *             Several probe implementations found in the classloader
     */
    public Monitoring(final Timer responseTimeProbesTimer, final long samplePeriod)
            throws MultipleProbesFactoriesFoundException, NoProbesFactoryFoundException {

        super(responseTimeProbesTimer, samplePeriod);

        final MacroProbesFactoryBuilder macroProbesFactoryBuilder = new MacroProbesFactoryBuilder();
        final MacroProbesFactory macroProbesFactory = macroProbesFactoryBuilder.getMacroProbesFactory();

        this.asyncExecutorThreadPoolDefectCreator = new AsyncExecutorThreadPoolDefectCreator(this);
        this.probeAsyncExecutorThreadPool = macroProbesFactory
                .createThreadPoolProbe(this.asyncExecutorThreadPoolDefectCreator);

    }

    /**
     * @param activitiEngine
     *            The Activiti BPMN Engine
     */
    public void setActivitiEngine(final ProcessEngine activitiEngine) {
        this.repositoryService = activitiEngine.getRepositoryService();
        this.runtimeService = activitiEngine.getRuntimeService();
        this.historyService = activitiEngine.getHistoryService();
    }

    /**
     * @param asyncExecutorTreadPool
     *            The asynchronous executor thread pool to monitor
     */
    public void setAsyncExecutorTreadPool(final ThreadPoolExecutor asyncExecutorTreadPool) {
        this.probeAsyncExecutorThreadPool.setThreadPool(asyncExecutorTreadPool);
    }

    @Override
    public void doInit() throws ProbeInitializedException, ProbeStartedException, ProbeInitializationException {
        this.probeAsyncExecutorThreadPool.init();
    }

    @Override
    protected void doStart() throws ProbeNotInitializedException, ProbeStartedException, ProbeStartupException {
        this.probeAsyncExecutorThreadPool.start();
    }

    @Override
    public void doStop() throws ProbeNotInitializedException, ProbeNotStartedException, ProbeStopException {
        this.probeAsyncExecutorThreadPool.stop();
    }

    @Override
    public void doShutdown() throws ProbeShutdownException, ProbeStartedException, ProbeNotInitializedException {
        this.probeAsyncExecutorThreadPool.shutdown();
    }

    @Override
    public Map<String, Long[]> getProcessDefinitions() {

        final List<ProcessDefinition> deployments = this.repositoryService.createProcessDefinitionQuery().list();
        final List<ProcessInstance> activeProcessInstances = this.runtimeService.createProcessInstanceQuery().active()
                .list();
        final List<ProcessInstance> suspendedProcessInstances = this.runtimeService.createProcessInstanceQuery()
                .suspended().list();
        final List<HistoricProcessInstance> endedProcessInstances = historyService.createHistoricProcessInstanceQuery()
                .finished().list();

        final Map<String, Long[]> results = new HashMap<>(deployments.size());
        for (final ProcessDefinition deployment : deployments) {
            final Long[] values = new Long[4];

            // Suspension state
            values[0] = deployment.isSuspended() ? 1l : 0l;

            // active process instances
            values[1] = Long.valueOf(activeProcessInstances.size());

            // suspended process instances
            values[2] = Long.valueOf(suspendedProcessInstances.size());

            // ended process instances
            values[3] = Long.valueOf(endedProcessInstances.size());

            results.put(deployment.getKey(), values);
        }
        return results;
    }

    //
    // --- Message exchange processor thread pool metrics
    //

    @Override
    public long getAsyncExecutorThreadPoolMaxSize() throws MonitoringServiceException {
        return this.probeAsyncExecutorThreadPool.getThreadPoolMaxSize();
    }

    @Override
    public long getAsyncExecutorThreadPoolMinSize() throws MonitoringServiceException {
        return this.probeAsyncExecutorThreadPool.getThreadPoolMinSize();
    }

    @Override
    public long getAsyncExecutorThreadPoolActiveThreadsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolActiveThreadsMax();
        } catch (final ProbeNotInitializedException e) {
            throw new MonitoringProbeNotInitializedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolActiveThreadsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolActiveThreadsCurrent();
        } catch (final ProbeNotStartedException e) {
            throw new MonitoringProbeNotStartedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolIdleThreadsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolIdleThreadsMax();
        } catch (final ProbeNotInitializedException e) {
            throw new MonitoringProbeNotInitializedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolIdleThreadsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolIdleThreadsCurrent();
        } catch (final ProbeNotStartedException e) {
            throw new MonitoringProbeNotInitializedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolQueuedRequestsMax()
            throws MonitoringProbeNotInitializedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolQueuedRequestsMax();
        } catch (final ProbeNotInitializedException e) {
            throw new MonitoringProbeNotInitializedException(e);
        }
    }

    @Override
    public long getAsyncExecutorThreadPoolQueuedRequestsCurrent()
            throws MonitoringProbeNotStartedException, MonitoringServiceException {
        try {
            return this.probeAsyncExecutorThreadPool.getThreadPoolQueuedRequestsCurrent();
        } catch (final ProbeNotStartedException e) {
            throw new MonitoringProbeNotStartedException(e);
        }
    }
}