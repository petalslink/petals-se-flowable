/**
 * Copyright (c) 2024 Linagora
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
package org.ow2.petals.flowable.junit.extensions.impl;

import static org.junit.platform.commons.util.AnnotationUtils.findAnnotatedFields;
import static org.junit.platform.commons.util.AnnotationUtils.findAnnotation;
import static org.junit.platform.commons.util.ReflectionUtils.makeAccessible;

import java.io.File;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Parameter;
import java.net.URISyntaxException;
import java.net.URL;
import java.sql.Driver;
import java.util.function.Predicate;

import org.junit.jupiter.api.extension.AnnotatedElementContext;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.ExtensionConfigurationException;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolutionException;
import org.junit.jupiter.api.extension.ParameterResolver;
import org.junit.platform.commons.JUnitException;
import org.junit.platform.commons.util.ExceptionUtils;
import org.junit.platform.commons.util.ReflectionUtils;
import org.ow2.petals.flowable.FlowableSEConstants.DBServer;
import org.ow2.petals.flowable.identity.SeFlowableIdmEngineConfigurator;
import org.ow2.petals.flowable.identity.file.FileIdmEngineConfigurator;
import org.ow2.petals.flowable.identity.ldap.LdapIdmEngineConfigurator;
import org.ow2.petals.flowable.junit.extensions.FlowableClientExtension;
import org.ow2.petals.flowable.junit.extensions.FlowableClientExtension.DatabaseType;
import org.ow2.petals.flowable.junit.extensions.FlowableClientExtension.IdmEngineType;
import org.ow2.petals.flowable.junit.extensions.api.FlowableClient;

/**
 * The implementation of JUnit 5 extension {@link FlowableClientExtension} that allows creation of Flowable client.
 * 
 * @see FlowableClientExtension
 */
public class FlowableClientExtensionImpl implements BeforeAllCallback, BeforeEachCallback, ParameterResolver {

    private static final ExtensionContext.Namespace NAMESPACE = ExtensionContext.Namespace
            .create(FlowableClientExtensionImpl.class);
    private static final Class<FlowableClient> KEY = FlowableClient.class;

    @Override
    public void beforeAll(final ExtensionContext context) throws Exception {
        this.injectStaticFields(context, context.getRequiredTestClass());
    }

    @Override
    public void beforeEach(final ExtensionContext context) throws Exception {
        context.getRequiredTestInstances().getAllInstances()
                .forEach(instance -> injectInstanceFields(context, instance));
    }

    private void injectStaticFields(final ExtensionContext context, final Class<?> testClass) {
        this.injectFields(context, null, testClass, ReflectionUtils::isStatic);
    }

    private void injectInstanceFields(final ExtensionContext context, final Object instance) {
        this.injectFields(context, instance, instance.getClass(), ReflectionUtils::isNotStatic);
    }

    private void injectFields(final ExtensionContext context, final Object testInstance, final Class<?> testClass,
            final Predicate<Field> predicate) {

        findAnnotatedFields(testClass, FlowableClientExtension.class, predicate).forEach(field -> {
            assertSupportedType("field", field.getType());

            try {
                final Class<? extends Driver> driver = this.determineJdbcDriverForField(field);
                final DatabaseType databaseType = this.determineDatabaseTypeForField(field);
                final String jdbcUrlPattern = this.determineJdbcUrlPatternForField(field);
                final String jdbcUser = this.determineJdbcUserForField(field);
                final String jdbcPassword = this.determineJdbcPasswordForField(field);
                final IdmEngineType idemEngineType = this.determineIdmEngineTypeForField(field);
                final File idmEngineConfiguratorCfgFile = this.determineIdmEngineConfiguratorCfgFileForField(field);
                final boolean autoStart = this.determineAutoStartForField(field);

                final FlowableClient flowablelient = this.createFlowableClient(driver, databaseType, jdbcUrlPattern,
                        jdbcUser, jdbcPassword, idemEngineType, idmEngineConfiguratorCfgFile, autoStart);

                // Register the field to be automatically closed
                final Namespace namespace = NAMESPACE.append(new FieldContext(field));
                context.getStore(namespace).put(KEY, flowablelient);

                // Set the field value
                makeAccessible(field).set(testInstance, flowablelient);

            } catch (final Throwable t) {
                ExceptionUtils.throwAsUncheckedException(t);
            }
        });
    }

    private FlowableClient createFlowableClient(final Class<? extends Driver> driver, final DatabaseType databaseType,
            final String jdbcUrlPattern, final String jdbcUser, final String jdbcPassword,
            final IdmEngineType idemEngineType, final File idmEngineConfiguratorCfgFile, final boolean autoStart)
            throws Exception {

        final SeFlowableIdmEngineConfigurator idmEngineConfigurator;
        if (idemEngineType == IdmEngineType.FILE_BASED) {
            idmEngineConfigurator = new FileIdmEngineConfigurator();
        } else {
            idmEngineConfigurator = new LdapIdmEngineConfigurator();
        }

        final FlowableClient flowableClient;
        if (databaseType == DatabaseType.IN_MEMORY) {
            flowableClient = new FlowableClientImpl(driver.getName(), jdbcUrlPattern, "flowable-test", jdbcUser,
                    jdbcPassword, idmEngineConfigurator, idmEngineConfiguratorCfgFile);
        } else {
            flowableClient = new FlowableClientImpl(driver.getName(), jdbcUrlPattern, null,
                    DBServer.DEFAULT_JDBC_URL_DATABASE_FILENAME, jdbcUser, jdbcPassword, idmEngineConfigurator,
                    idmEngineConfiguratorCfgFile);
        }

        // Initialize the Flowable client
        flowableClient.init();

        // Start the Flowable client (ie. its Flowable engines)
        if (autoStart) {
            flowableClient.start();
        }

        return flowableClient;

    }

    private void assertSupportedType(String target, Class<?> type) {
        if (type != FlowableClient.class) {
            throw new ExtensionConfigurationException("Can only resolve @" + FlowableClientExtension.class.getName()
                    + " " + target + " of type " + FlowableClient.class.getName() + " but was: " + type.getName());
        }
    }

    private Class<? extends Driver> determineJdbcDriverForField(final Field field) {
        final FlowableClientExtension flowableClient = findAnnotation(field, FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException(
                        "Field " + field + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.jdbcDriver();
    }

    private DatabaseType determineDatabaseTypeForField(final Field field) {
        final FlowableClientExtension flowableClient = findAnnotation(field, FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException(
                        "Field " + field + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.databaseType();
    }

    private String determineJdbcUserForField(final Field field) {
        final FlowableClientExtension flowableClient = findAnnotation(field, FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException(
                        "Field " + field + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.jdbcUser();
    }

    private String determineJdbcPasswordForField(final Field field) {
        final FlowableClientExtension flowableClient = findAnnotation(field, FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException(
                        "Field " + field + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.jdbcPassword();
    }

    private String determineJdbcUrlPatternForField(final Field field) {
        final FlowableClientExtension flowableClient = findAnnotation(field, FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException(
                        "Field " + field + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.jdbcUrlPattern();
    }

    private IdmEngineType determineIdmEngineTypeForField(final Field field) {
        final FlowableClientExtension flowableClient = findAnnotation(field, FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException(
                        "Field " + field + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.idmEngineType();
    }

    private File determineIdmEngineConfiguratorCfgFileForField(final Field field) throws URISyntaxException {
        final FlowableClientExtension flowableClient = findAnnotation(field, FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException(
                        "Field " + field + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return this.determineIdmEngineConfiguratorCfgFile(flowableClient.idmEngineConfiguratorCfgFile());
    }

    private File determineIdmEngineConfiguratorCfgFile(final String cfgFile) throws URISyntaxException {
        if (cfgFile.isBlank()) {
            return new File(FlowableClient.IDM_ENGINE_CONFIGURATION_TO_GENERATE);
        } else if (cfgFile.equals(FlowableClient.DEFAULT_IDM_ENGINE_CONFIGURATION)) {
            return null;
        } else {
            final URL cfgFileAsResource = Thread.currentThread().getContextClassLoader().getResource(cfgFile);
            if (cfgFileAsResource != null) {
                return new File(cfgFileAsResource.toURI());
            } else {
                return new File(cfgFile);
            }
        }
    }

    private boolean determineAutoStartForField(final Field field) {
        final FlowableClientExtension flowableClient = findAnnotation(field, FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException(
                        "Field " + field + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.autoStart();
    }

    private static class FieldContext implements AnnotatedElementContext {

        private final Field field;

        private FieldContext(final Field field) {
            this.field = field;
        }

        @Override
        public AnnotatedElement getAnnotatedElement() {
            return this.field;
        }
    }

    /**
     * Determine if the {@link Parameter} in the supplied {@link ParameterContext} is annotated with
     * {@link FlowableClientExtension @FlowableClientExtension}.
     */
    @Override
    public boolean supportsParameter(final ParameterContext parameterContext, final ExtensionContext extensionContext) {
        final boolean annotated = parameterContext.isAnnotated(FlowableClientExtension.class);
        if (annotated && parameterContext.getDeclaringExecutable() instanceof Constructor) {
            throw new ParameterResolutionException(
                    "@FlowableClientExtension is not supported on constructor parameters. Please use field injection instead.");
        }
        return annotated;
    }

    @Override
    public Object resolveParameter(final ParameterContext parameterContext, final ExtensionContext context)
            throws ParameterResolutionException {
        final Class<?> parameterType = parameterContext.getParameter().getType();
        assertSupportedType("parameter", parameterType);

        try {
            // Create the Petals JMX API implementation
            final Class<? extends Driver> driver = this.determineJdbcDriverForParameter(parameterContext);
            final DatabaseType databaseType = this.determineDatabaseTypeForParameter(parameterContext);
            final String jdbcUrlPattern = this.determineJdbcUrlPatternForParameter(parameterContext);
            final String jdbcUser = this.determineJdbcUserForParameter(parameterContext);
            final String jdbcPassword = this.determineJdbcPasswordForParameter(parameterContext);
            final IdmEngineType idemEngineType = this.determineIdmEngineTypeForParameter(parameterContext);
            final File idmEngineConfiguratorCfgFile = this
                    .determineIdmEngineConfiguratorCfgFileForParameter(parameterContext);
            final boolean autoStart = this.determineAutoStartForParameter(parameterContext);

            final FlowableClient flowablelient = this.createFlowableClient(driver, databaseType, jdbcUrlPattern,
                    jdbcUser, jdbcPassword, idemEngineType, idmEngineConfiguratorCfgFile, autoStart);

            // Register the parameter to be automatically closed
            final Namespace namespace = NAMESPACE.append(parameterContext);
            context.getStore(namespace).put(KEY, flowablelient);

            return flowablelient;
        } catch (final Exception e) {
            throw new ParameterResolutionException("Unable to create the Flowable client", e);
        }
    }

    private Class<? extends Driver> determineJdbcDriverForParameter(final ParameterContext parameterContext) {
        final FlowableClientExtension flowableClient = parameterContext.findAnnotation(FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException("Parameter " + parameterContext.getParameter()
                        + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.jdbcDriver();
    }

    private DatabaseType determineDatabaseTypeForParameter(final ParameterContext parameterContext) {
        final FlowableClientExtension flowableClient = parameterContext.findAnnotation(FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException("Parameter " + parameterContext.getParameter()
                        + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.databaseType();
    }

    private String determineJdbcUserForParameter(final ParameterContext parameterContext) {
        final FlowableClientExtension flowableClient = parameterContext.findAnnotation(FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException("Parameter " + parameterContext.getParameter()
                        + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.jdbcUser();
    }

    private String determineJdbcPasswordForParameter(final ParameterContext parameterContext) {
        final FlowableClientExtension flowableClient = parameterContext.findAnnotation(FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException("Parameter " + parameterContext.getParameter()
                        + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.jdbcPassword();
    }

    private String determineJdbcUrlPatternForParameter(final ParameterContext parameterContext) {
        final FlowableClientExtension flowableClient = parameterContext.findAnnotation(FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException("Parameter " + parameterContext.getParameter()
                        + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.jdbcUrlPattern();
    }

    private IdmEngineType determineIdmEngineTypeForParameter(final ParameterContext parameterContext) {
        final FlowableClientExtension flowableClient = parameterContext.findAnnotation(FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException("Parameter " + parameterContext.getParameter()
                        + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.idmEngineType();
    }

    private File determineIdmEngineConfiguratorCfgFileForParameter(final ParameterContext parameterContext)
            throws URISyntaxException {
        final FlowableClientExtension flowableClient = parameterContext.findAnnotation(FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException("Parameter " + parameterContext.getParameter()
                        + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return this.determineIdmEngineConfiguratorCfgFile(flowableClient.idmEngineConfiguratorCfgFile());
    }

    private boolean determineAutoStartForParameter(final ParameterContext parameterContext) {
        final FlowableClientExtension flowableClient = parameterContext.findAnnotation(FlowableClientExtension.class)
                .orElseThrow(() -> new JUnitException("Parameter " + parameterContext.getParameter()
                        + " must be annotated with @" + FlowableClientExtension.class.getName()));
        return flowableClient.autoStart();
    }
}
