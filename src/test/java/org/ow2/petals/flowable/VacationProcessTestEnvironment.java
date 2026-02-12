/**
 * Copyright (c) 2014-2026 Linagora
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
package org.ow2.petals.flowable;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_GROUP_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_GROUP_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_USER_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_USER_SERVICE;

import java.io.File;
import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.NativeServiceConfigurationFactory;
import org.ow2.petals.component.framework.junit.rule.ParameterGenerator;
import org.ow2.petals.flowable.outgoing.WSDLImporterForFlowableFactory;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 */
public abstract class VacationProcessTestEnvironment extends AbstractVacationProcessTestEnvironment {

    @BeforeAll
    private static void completesTestEnvConfiguration() throws Exception {
        completesComponentUnderTestConfiguration();
        completesFlowableClientConfiguration();
    }

    private static void completesFlowableClientConfiguration() throws Exception {
        FLOWABLE_CLIENT.start();

        // When purging all process definitions, WSDLs are imported. So we must use the right WSDL importer
        FLOWABLE_CLIENT.setXMLImporterFactory(
                new WSDLImporterForFlowableFactory(COMPONENT_UNDER_TEST.getComponentObject().getServiceUnitManager()));
    }

    private static void completesComponentUnderTestConfiguration() throws Exception {
        COMPONENT_UNDER_TEST
                .setParameter(
                        new QName(FlowableSEConstants.NAMESPACE_COMP,
                                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE),
                        // Generate identity service configuration files
                        new ParameterGenerator() {

                            @Override
                            public String generate() throws Exception {
                                final URL idmEngineConfiguratorCfg = Thread.currentThread().getContextClassLoader()
                                        .getResource(VACATION_SU_HOME + "idm-engine-configurator.properties");
                                assertNotNull(idmEngineConfiguratorCfg,
                                        "IDM engine configurator config file is missing !");
                                return new File(idmEngineConfiguratorCfg.toURI()).getAbsolutePath();
                            }

                        })
                .registerServiceToDeploy(VACATION_SU, createVacationServiceCfgFactory())
                .registerNativeServiceToDeploy(NATIVE_USER_SVC_CFG, new NativeServiceConfigurationFactory() {

                    @Override
                    public ServiceConfiguration create(final String nativeEndpointName) {

                        final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("component.wsdl");
                        assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                        return new ProvidesServiceConfiguration(ITG_USER_PORT_TYPE, ITG_USER_SERVICE,
                                nativeEndpointName, nativeServiceWsdlUrl);
                    }

                    @Override
                    public QName getNativeService() {
                        return ITG_USER_SERVICE;
                    }
                }).registerNativeServiceToDeploy(NATIVE_GROUP_SVC_CFG, new NativeServiceConfigurationFactory() {

                    @Override
                    public ServiceConfiguration create(final String nativeEndpointName) {

                        final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("component.wsdl");
                        assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                        return new ProvidesServiceConfiguration(ITG_GROUP_PORT_TYPE, ITG_GROUP_SERVICE,
                                nativeEndpointName, nativeServiceWsdlUrl);
                    }

                    @Override
                    public QName getNativeService() {
                        return ITG_GROUP_SERVICE;
                    }
                }).registerNativeServiceToDeploy(NATIVE_TASKS_SVC_CFG, new NativeServiceConfigurationFactory() {

                    @Override
                    public ServiceConfiguration create(final String nativeEndpointName) {

                        final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("component.wsdl");
                        assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                        return new ProvidesServiceConfiguration(ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                                nativeEndpointName, nativeServiceWsdlUrl);
                    }

                    @Override
                    public QName getNativeService() {
                        return ITG_TASK_SERVICE;
                    }
                }).registerNativeServiceToDeploy(NATIVE_PROCESSINSTANCES_SVC_CFG,
                        new NativeServiceConfigurationFactory() {

                            @Override
                            public ServiceConfiguration create(final String nativeEndpointName) {

                                final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                        .getResource("component.wsdl");
                                assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                                return new ProvidesServiceConfiguration(ITG_PROCESSINSTANCES_PORT_TYPE,
                                        ITG_PROCESSINSTANCES_SERVICE, nativeEndpointName, nativeServiceWsdlUrl);
                            }

                            @Override
                            public QName getNativeService() {
                                return ITG_PROCESSINSTANCES_SERVICE;
                            }
                        })
                .registerNativeServiceToDeploy(NATIVE_EXECUTIONS_SVC_CFG, new NativeServiceConfigurationFactory() {

                    @Override
                    public ServiceConfiguration create(final String nativeEndpointName) {

                        final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource("component.wsdl");
                        assertNotNull(nativeServiceWsdlUrl, "Integration servce WSDl not found");
                        return new ProvidesServiceConfiguration(ITG_EXECUTIONS_PORT_TYPE, ITG_EXECUTIONS_SERVICE,
                                nativeEndpointName, nativeServiceWsdlUrl);
                    }

                    @Override
                    public QName getNativeService() {
                        return ITG_EXECUTIONS_SERVICE;
                    }
                }).registerExternalServiceProvider(ARCHIVE_ENDPOINT, ARCHIVE_SERVICE, ARCHIVE_INTERFACE)
                .postInitComponentUnderTest();
    }
}
